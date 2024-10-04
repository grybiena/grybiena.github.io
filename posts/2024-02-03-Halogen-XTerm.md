[xterm.js](https://github.com/xtermjs/xterm.js) is a front-end component written in TypeScript implementing a fully featured terminal for the browser. I have published a complete set of purescript bindings available on pursuit [xterm](https://pursuit.purescript.org/packages/purescript-xterm/1.0.0). In addition to this I have published a halogen component [halogen-xterm](https://pursuit.purescript.org/packages/purescript-halogen-xterm/2.0.0) also available on pursuit.

The halogen component enables embedding of a terminal application into a halogen UI. This could be a connection to a remote shell or simply an integrated element of your UI.

To make creationg custom command lines for your web-app as easy as possible the halogen-xterm library contains a wrapper component called XShell. 

XShell allows you to create a command line shell that integrates with your web-app by specifying a set of commands [like this](https://github.com/grybiena/grybiena.github.io/blob/grybiena/examples/halogen-xterm/src/Examples/Halogen/XTerm/Component.purs).

```haskell
commands :: forall o m. MonadAff m => MonadRec m => Array (Command (Windows m) o m)
commands = 
  [ { name: "button"
    , description: [ "creates a button as a subprocess."
                   , "the subprocess can be cancelled with ^C"
                   , "accepts button text as argument."
                   , "clicking the button prints \"click!\" to the terminal."
                   ]
    , cmd: \args -> do 
        modifyShell (cmd .~ "")
        openWindow _button unit Button.component (joinWith " " args) (const $ terminal $ writeLn "click!")
        let proc =  { stdin: \txt -> void $ queryWindow _button unit (SetText txt unit) 
                    , kill: closeWindow _button unit
                    }
        modifyShell (\(ShellState s) -> ShellState s { foreground = Just proc })
        terminal do
           options $ setCursorBlink false
           write "\r\n"
        interpreter canceler
    } 
  ]
```

Try the example below. Type "help" to see a list of available commands.

<iframe width=750px height=900px src="/examples/halogen-xterm/index.html">
