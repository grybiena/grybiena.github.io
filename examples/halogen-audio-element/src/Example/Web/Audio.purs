module Example.Web.Audio where

import Prelude hiding (top)

import CSS (em, fontFamily, height, pct, width)
import CSS.Font (monospace)
import Control.Monad.Rec.Class (class MonadRec)
import Data.Array ((:))
import Data.Maybe (Maybe(..))
import Data.NonEmpty (singleton)
import Data.Number as Number
import Data.Traversable (traverse_)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.Audio.Element as Audio
import Halogen.Audio.Element.Event (AudioElementEvent(..))
import Halogen.HTML as HH
import Halogen.HTML.CSS (style)
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties (InputType(..))
import Halogen.HTML.Properties as HP
import Type.Prelude (Proxy(..))

type Slots = ( audio :: H.Slot Audio.Query Audio.Output Unit) 

_audio = Proxy :: Proxy "audio"

component :: forall q i o m . MonadAff m => MonadRec m => H.Component q i o m 
component =
  H.mkComponent
    { initialState: const { events: [], time: 0.0, duration: 0.0, volume: 1.0, muted: false, source: "", playing: false } 
    , render
    , eval: H.mkEval $ H.defaultEval { initialize = Just Initialize, handleAction = handleAction } 
    }

type State =
  { events :: Array AudioElementEvent
  , time :: Number
  , duration :: Number
  , volume :: Number
  , muted :: Boolean
  , source :: String
  , playing :: Boolean
  }

data Action =
    Initialize 
  | AudioEvent Audio.Output
  | ToggleControls Boolean 
  | PlayPause
  | SeekTo (Maybe Number)


handleAction :: forall o m. MonadEffect m => Action -> H.HalogenM State Action Slots o m Unit
handleAction Initialize = do
  pure unit
handleAction (AudioEvent (Audio.AudioElement _)) = do
  pure unit
handleAction (AudioEvent (Audio.AudioElementEvent e)) = do
  H.modify_ (\st -> st { events = e:st.events })
  case e of
    LoadedMetaData -> do
      t <- H.query _audio unit (Audio.GetSrc identity)
      flip traverse_ t $ \t' -> do
         H.modify_ (\st -> st { source = t' })
    TimeUpdate -> do
      t <- H.query _audio unit (Audio.GetCurrentTime identity)
      flip traverse_ t $ \t' -> do
         H.modify_ (\st -> st { time = t' })
    DurationChange -> do
      t <- H.query _audio unit (Audio.Duration identity)
      flip traverse_ t $ \t' -> do
         H.modify_ (\st -> st { duration = t' })
    VolumeChange -> do
      t <- H.query _audio unit (Audio.GetVolume identity)
      m <- H.query _audio unit (Audio.Muted identity)
      flip traverse_ (Tuple <$> t <*> m) $ \(t' /\ m') -> do
         H.modify_ (\st -> st { volume = t', muted = m' })
    Play -> do
       H.modify_ (\st -> st { playing = true })
    Pause -> do
       H.modify_ (\st -> st { playing = false })
    _ -> pure unit    
handleAction (ToggleControls b) = do
  H.tell _audio unit (Audio.Controls b)
handleAction PlayPause = do
  playing <- H.gets (\st -> st.playing)
  if playing
    then H.tell _audio unit Audio.Pause
    else H.tell _audio unit Audio.Play
handleAction (SeekTo n) = flip traverse_ n $ \t -> do
  H.tell _audio unit (Audio.SetCurrentTime t)



render :: forall m. MonadEffect m => State -> H.ComponentHTML Action Slots m
render st = HH.div
  [ style do
      fontFamily [] (singleton monospace) 
  ]
  [ HH.h3_ [ HH.text "Audio Element Example" ]
  , HH.div_ [ HH.text ("source: " <> st.source) ]
  , HH.div_ [ HH.text ("time: " <> show st.time <> "s") ]
  , HH.div_ [ HH.text ("duration: " <> show st.duration <> "s") ]
  , HH.div_ [ HH.text ("volume: " <> show st.volume) ]
  , HH.div_ [ HH.text ("muted: " <> show st.muted) ]
  , HH.div
      [ style do
          height (em 2.0)

      ] []
  , HH.slot _audio unit Audio.component { controls: true, source: "/audio/bubbles.mp3" } AudioEvent
  , HH.div_ [ HH.input [ HP.checked true
                       , HP.type_ InputCheckbox
                       , HE.onChecked ToggleControls 
                       ], HH.text "Show Default Controls"  ]
  , HH.div_ [ HH.button
                [ HE.onClick (const PlayPause)
                ]
                [ HH.text (if st.playing then "Pause" else "Play")
                ]
            , HH.input
                [ HP.type_ InputRange
                , HP.min 0.0
                , HP.max 1000.0 
                , HP.value (show (st.time*1000.0/st.duration))
                , HE.onValueInput (\s -> SeekTo ((\n -> n*st.duration/1000.0) <$> (Number.fromString s)))
                , style do
                    width (pct 80.0)
                ]
            ]
  , HH.div_ ((HH.h4_ [HH.text "Audio Element Events"]):(renderEvent <$> st.events)) 
  ]
  where
    renderEvent e = HH.p_ [ HH.text (show e) ] 


