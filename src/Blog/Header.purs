module Blog.Header where

import Prelude

import CSS (backgroundColor, fromHexString)
import Color as Color
import Data.Maybe (Maybe(..), maybe)
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS (style)

component :: forall q i o . H.Component q i o Aff
component =
  H.mkComponent
    { initialState: const unit 
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction
                                     , initialize = Just Initialize
                                     }
    }

data Action = Initialize

handleAction :: forall m. Applicative m => Action -> m Unit 
handleAction _ = pure unit

type Slots :: forall k. Row k
type Slots = ()

render :: Unit -> H.ComponentHTML Action Slots Aff
render _ = HH.div
  [ style do
      backgroundColor (maybe Color.white identity (fromHexString "#ABEBC6")) 
  ] [ HH.h1 [] [ HH.text "blog.grybiena.com" ] ]


