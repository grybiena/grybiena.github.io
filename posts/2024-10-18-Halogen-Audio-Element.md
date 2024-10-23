[This library](https://github.com/grybiena/halogen-audio-element) exposes a halogen component wrapping the API of the web audio element. The [\<audio\>](https://developer.mozilla.org/en-US/docs/Web/HTML/Element/audio) element emits events detailing information about player state changes. These events are transformed into outputs of the halogen component and the operations that can be performed on the element are represented by the Query AST of the halogen component.

There is a neat trick for binding to all of the event types on initialization.

```haskell
...
  Initialize -> do
    e <- H.getRef (H.RefLabel "audio")
    flip traverse_ (e >>= fromElement) $ \el -> do
      traverse_ (subscribeToEvent el) audioElementEvents 
      H.raise $ AudioElement el
```
The event types are [encoded as a data type](https://github.com/grybiena/halogen-audio-element/blob/182a0f438848f89e28c8f08432ea3dc7420b22f4/src/Halogen/Audio/Element/Event.purs#L16) which is a _Bounded_ _Enum_ so can be enumerated fully. The lower case string reresentation of each constructor is the name of the event type. This means we can traverse over all the event types to bind event listeners for each event.

```haskell
subscribeToEvent :: forall o s.
                    MonadEffect m
                 => HTMLAudioElement -> AudioElementEvent -> H.HalogenM s Action () o m Unit 
subscribeToEvent el t = do
  { emitter, listener } <- H.liftEffect $ HS.create
  callback <- H.liftEffect $ eventListener (const $ HS.notify listener t)
  H.liftEffect $ addEventListener (EventType $ toLower $ show t) callback false (toEventTarget el)
  void $ H.subscribe (Bubble <$> emitter)

-- this is generic despite the specific name
audioElementEvents :: forall e . Enum e => Bounded e => Array e
audioElementEvents = unfoldr (\a -> Tuple a <$> succ a) bottom 
```

The below [example](https://github.com/grybiena/grybiena.github.io/tree/grybiena/examples/halogen-audio-element) demonstrates the component by logging audio events underneath and updating some playback information in an event driven manner. The API of the audio element has sufficient features to allow disabling of the default controls and implementation of a custom audio playback interface (more on that later).


<iframe width=750px height=500px src="/examples/halogen-audio-element/index.html">
