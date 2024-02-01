module Main where

import Prelude hiding (top)

import CSS (black, border, borderColor, display, flex, flexDirection, padding, px, solid)
import CSS.Common (center)
import CSS.Flexbox (alignItems, column, flexWrap, nowrap, row)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (class MonadEffect)
import Examples.Halogen.Infinite.Scroll.Main as InfiniteScroll
import Examples.Halogen.XTerm.Component as XTerm
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.CSS (style)
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)
import Web.Event.Event (Event, target)


main :: Effect Unit
main = do
  HA.runHalogenAff do
    body <- HA.awaitBody
    void $ runUI component unit body

type Slots = ( infiniteScroll :: forall q o. H.Slot q o Unit
             , xterm :: forall q o. H.Slot q o Unit
             ) 

_infiniteScroll = Proxy :: Proxy "infiniteScroll"
_xterm = Proxy :: Proxy "xterm"

type State =
  { example :: Example
  }
  

component :: forall q o . H.Component q Unit o Aff
component =
  H.mkComponent
    { initialState: const { example: InfiniteScroll } 
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }

data Example =
    InfiniteScroll
  | XTerm

derive instance Generic Example _

instance Show Example where
  show = genericShow

data Action =
    SelectExample Example

selectExample :: Event -> Action
selectExample e =
  case (\x -> (unsafeCoerce x).value) <$> target e of
    Just "InfiniteScroll" -> SelectExample InfiniteScroll
    _ -> SelectExample XTerm

handleAction :: forall o m. MonadEffect m => Action -> H.HalogenM State Action Slots o m Unit 
handleAction =
  case _ of
    SelectExample e -> H.modify_ (\st -> st { example = e }) 

selector :: Example -> H.ComponentHTML Action Slots Aff 
selector e = 
  HH.select
    [ HP.value "InfiniteScroll"
    , HE.onChange selectExample
    ]
    [ HH.option
        [ HP.value "InfiniteScroll"
        ]
        [ HH.text "InfiniteScroll"
        ]
    , HH.option
        [ HP.value "XTerm"
        ]
        [ HH.text "XTerm"
        ]
    ]


render :: State -> H.ComponentHTML Action Slots Aff 
render t =
  HH.div
    [ style do
        display flex
        flexDirection column 
    ]
    [ HH.div
        [ style do
            display flex
            flexDirection row 
            alignItems center
            border solid (px 1.0) black
        ]
        [ HH.h3_ [ HH.text "Examples" ]
        , HH.div [ style $ padding (px 0.0) (px 0.0) (px 0.0) (px 30.0)] [ selector t.example ]
        ]
    , HH.div
        [ style do
            border solid (px 1.0) black
        ]
        [ case t.example of
            InfiniteScroll -> HH.slot_ _infiniteScroll unit InfiniteScroll.feedComponent unit
            XTerm -> HH.slot_ _xterm unit XTerm.component unit
        ]
    ]
 
