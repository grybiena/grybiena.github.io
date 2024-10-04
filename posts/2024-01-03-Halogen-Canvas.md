The web [Canvas API](https://developer.mozilla.org/en-US/docs/Web/API/Canvas_API) is a lovely thing to play with. I wanted to expose it as a Halogen component with an API that inspires joy. What better way to do that than with a DSL defined by [Free Monad](https://www.haskellforall.com/2012/06/you-could-have-invented-free-monads.html) over the operations provided by the API. This component is available as a package on [pursuit](https://pursuit.purescript.org/packages/purescript-halogen-canvas/1.0.0/docs/Halogen.Canvas).

The [query type of a Halogen component](https://purescript-halogen.github.io/purescript-halogen/guide/05-Parent-Child-Components.html#queries) is [conveniently shaped](https://github.com/grybiena/halogen-canvas/blob/79887794f8ad7f10889d67dfbc0ec0b5c99a6e85/src/Halogen/Canvas.purs#L64) to accept a [Free Monad AST](https://pursuit.purescript.org/packages/purescript-halogen-canvas/1.0.0/docs/Graphics.Canvas.Free). That means that abstracting the features of the canvas API into a Free Monad gives us a convenient way to control a canvas with an embedded DSL.

This [snippet of code]( https://github.com/grybiena/grybiena.github.io/blob/8c08316b3cea003532e34feaca48b16a8a2fd6ec/examples/halogen-canvas/src/Examples/Halogen/Canvas/Sketch.purs#L105) queries the *slot* named *_sketch* (the canvas component) defined in the example. The form of the query is just some code in the CanvasT (free) monad. I like how elegantly the Halogen library can be extended in this way by defining DSLs that are interpreted by components.

```haskell
H.query _sketch unit do
  withContext do
    width <- getWidth
    height <- getHeight
    clearRect { x: 0.0, y: 0.0, width, height }
    draw s
```

Here we [begin a new context](https://github.com/grybiena/halogen-canvas/blob/79887794f8ad7f10889d67dfbc0ec0b5c99a6e85/src/Graphics/Canvas/Free.purs#L456-L456), clear the canvas, and draw the state of the sketch. The draw routine for the example at the end of this post is shown below.  

```haskell
draw :: forall m. State -> CanvasT m Unit
draw w = do
  setFillColor $ rgb 255 255 200
  fillRect { x: 0.0, y: 0.0, width: 400.0, height: 400.0 }
  setLineWidth 5.0
  void $ flip traverseWithIndex (maybe w.lines (flip cons w.lines) w.line) $ \i (f /\ t) -> do
     beginPath
     moveTo f
     lineTo t
     if (Just i == w.selected)
       then setStrokeColor $ rgb 255 0 0
       else setStrokeColor $ rgb 0 0 0
     stroke
```

For examples like this where the canvas is something to be interacted with the [interact](https://pursuit.purescript.org/packages/purescript-halogen-canvas/1.0.0/docs/Halogen.Canvas.Interact) component is provided which outputs keyboard, mouse, and touch events to be handled by the parent component. Also available are the vanilla [canvas](https://pursuit.purescript.org/packages/purescript-halogen-canvas/1.0.0/docs/Halogen.Canvas.Interact) component which lacks this output for cases where it is unneeded, and the [animate](https://pursuit.purescript.org/packages/purescript-halogen-canvas/1.0.0/docs/Halogen.Canvas.Interact) component which contains an animation loop driven by [requestAnimationFrame](https://developer.mozilla.org/en-US/docs/Web/API/Window/requestAnimationFrame) for simple animations indexed by time.

#### Sketch example
Click and drag on the canvas to draw a line.
<iframe width=750px height=500px src="/examples/halogen-canvas/index.html">
