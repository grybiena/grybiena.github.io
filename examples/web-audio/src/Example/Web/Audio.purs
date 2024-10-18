module Example.Web.Audio where

import Prelude hiding (top)

import CSS (em, fontFamily, height)
import CSS.Font (monospace)
import Control.Monad.Rec.Class (class MonadRec)
import Data.Array ((:))
import Data.NonEmpty (singleton)
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.Audio.Element as Audio
import Halogen.Audio.Element.Event (AudioElementEvent)
import Halogen.HTML as HH
import Halogen.HTML.CSS (style)
import Type.Prelude (Proxy(..))

type Slots = ( audio :: H.Slot Audio.Query Audio.Output Unit) 

_audio = Proxy :: Proxy "audio"

component :: forall q i o m . MonadAff m => MonadRec m => H.Component q i o m 
component =
  H.mkComponent
    { initialState: const [] 
    , render
    , eval: H.mkEval $ H.defaultEval { initialize = Just Initialize, handleAction = handleAction } 
    }

data Action =
    Initialize 
  | AudioEvent Audio.Output

handleAction :: forall o m. MonadEffect m => Action -> H.HalogenM (Array AudioElementEvent) Action Slots o m Unit
handleAction Initialize = do
  pure unit
handleAction (AudioEvent (Audio.AudioElement _)) = do
  pure unit
handleAction (AudioEvent (Audio.AudioElementEvent e)) = do
  H.modify_ (\st -> e:st)
  pure unit

render :: forall m. MonadEffect m => Array AudioElementEvent -> H.ComponentHTML Action Slots m
render evts = HH.div
  [ style do
      fontFamily [] (singleton monospace) 
  ]
  [
    HH.div
      [ style do
          height (em 5.0)

      ] []
  , HH.slot _audio unit Audio.component { controls: true, source: "/audio/bubbles.mp3" } AudioEvent
  , HH.div_ (renderEvent <$> evts)      
  ]
  where
    renderEvent e = HH.p_ [ HH.text (show e) ] 


