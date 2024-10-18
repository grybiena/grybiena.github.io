module Example.Web.Audio where

import Prelude hiding (top)

import CSS (em, fontFamily, height)
import CSS.Font (monospace)
import Control.Monad.Rec.Class (class MonadRec)
import Data.Array ((:))
import Data.Maybe (Maybe(..))
import Data.NonEmpty (singleton)
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
import Type.Prelude (Proxy(..))

type Slots = ( audio :: H.Slot Audio.Query Audio.Output Unit) 

_audio = Proxy :: Proxy "audio"

component :: forall q i o m . MonadAff m => MonadRec m => H.Component q i o m 
component =
  H.mkComponent
    { initialState: const { events: [], time: 0.0, duration: 0.0, volume: 1.0, muted: false } 
    , render
    , eval: H.mkEval $ H.defaultEval { initialize = Just Initialize, handleAction = handleAction } 
    }

type State =
  { events :: Array AudioElementEvent
  , time :: Number
  , duration :: Number
  , volume :: Number
  , muted :: Boolean
  }

data Action =
    Initialize 
  | AudioEvent Audio.Output

handleAction :: forall o m. MonadEffect m => Action -> H.HalogenM State Action Slots o m Unit
handleAction Initialize = do
  pure unit
handleAction (AudioEvent (Audio.AudioElement _)) = do
  pure unit
handleAction (AudioEvent (Audio.AudioElementEvent e)) = do
  H.modify_ (\st -> st { events = e:st.events })
  case e of
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
    _ -> pure unit    
  pure unit

render :: forall m. MonadEffect m => State -> H.ComponentHTML Action Slots m
render st = HH.div
  [ style do
      fontFamily [] (singleton monospace) 
  ]
  [ HH.h3_ [ HH.text "Audio Element Example" ]
  , HH.div_ [ HH.text ("time: " <> show st.time <> "s") ]
  , HH.div_ [ HH.text ("duration: " <> show st.duration <> "s") ]
  , HH.div_ [ HH.text ("volume: " <> show st.volume) ]
  , HH.div_ [ HH.text ("muted: " <> show st.muted) ]
  , HH.div
      [ style do
          height (em 2.0)

      ] []
  , HH.slot _audio unit Audio.component { controls: true, source: "/audio/bubbles.mp3" } AudioEvent
  , HH.div_ ((HH.h4_ [HH.text "Audio Element Events"]):(renderEvent <$> st.events)) 
  ]
  where
    renderEvent e = HH.p_ [ HH.text (show e) ] 


