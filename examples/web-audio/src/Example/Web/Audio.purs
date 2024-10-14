module Example.Web.Audio where

import Prelude hiding (top)

import CSS (em, height)
import Control.Monad.Rec.Class (class MonadRec)
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.Audio.Element as Audio
import Halogen.HTML as HH
import Halogen.HTML.CSS (style)
import Type.Prelude (Proxy(..))

type Slots = ( audio :: H.Slot Audio.Query Audio.Output Unit) 

_audio = Proxy :: Proxy "audio"

component :: forall q i o m . MonadAff m => MonadRec m => H.Component q i o m 
component =
  H.mkComponent
    { initialState: const unit 
    , render
    , eval: H.mkEval $ H.defaultEval { initialize = Just Initialize, handleAction = handleAction } 
    }

data Action =
  Initialize 

handleAction :: forall s o m. MonadEffect m => Action -> H.HalogenM s Action Slots o m Unit
handleAction Initialize = do
  pure unit

render :: forall s m. MonadEffect m => s -> H.ComponentHTML Action Slots m
render _ = HH.div_ [
    HH.div
      [ style do
          height (em 5.0)
      ] []
  , HH.slot_ _audio unit Audio.component { controls: true, source: "/audio/bubbles.mp3" }
  ]

