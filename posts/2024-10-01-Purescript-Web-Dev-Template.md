This blog is a purescript UI hosted by github pages. It renders the blog posts using [markdown-it](https://github.com/markdown-it/markdown-it) for formatting and [highlightjs](https://github.com/highlightjs/highlight.js) for code highlighting.

The starting point for development was my [purescipt web dev template](https://github.com/grybiena/ps-webdev-template). This template contains a development server and frontend app that reloads the page whenever the source code changes.

### How does it do that?

The reload is triggered by the development server. It sends a message over a websocket connection to the front end app which reloads the page.

Here's the front end code that handles the reloading of the page. The reloading is handled in [Main.purs](https://github.com/grybiena/ps-webdev-template/blob/main/src/Main.purs) which runs the template [halogen app](https://github.com/grybiena/ps-webdev-template/blob/main/src/App.purs).

```haskell
main :: Effect Unit
main = do
  reloadHandler
  app

reloadHandler :: Effect Unit
reloadHandler = do
  http_url <- window >>= location >>= host 
  let ws_url = "ws://" <> http_url <> "/ws" 
  ws <- WS.create ws_url []
  let reloadPage = const $ window >>= location >>= reload
  messageListener <- eventListener reloadPage 
  addEventListener onMessage messageListener true (WS.toEventTarget ws)
```

The [development server](https://github.com/grybiena/ps-webdev-template/blob/main/dev-server.py) is written in python. It watches the ./src directory, recompiles the purescript app when the source changes, and broadcasts a reload message to all connected websocket clients after compilation.

Here's the relevant python code for watching and broadcasting a reload event. 

```python
async def websocket_handler(request):
    ws = web.WebSocketResponse()
    await ws.prepare(request)
    CLIENTS.add(ws)
    async for msg in ws:
        if msg.type == aiohttp.WSMsgType.ERROR:
            print('ws connection closed with exception %s' % ws.exception())
            CLIENTS.remove(ws)
    return ws

async def watch_recompile():
   async for _ in awatch("./src"):
       subprocess.run(["spago","bundle-app","-y"])
       await broadcast("reload")

async def broadcast(message):
    for websocket in CLIENTS.copy():
        try:
            await websocket.send_str(message)
        except:
            CLIENTS.remove(websocket)
```

If you are interested in trying out purescript for front end development then this template might be a good starting point.
