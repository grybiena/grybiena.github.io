module Main where

import Prelude hiding (top)

import CSS (black, border, display, flex, flexDirection, padding, px, solid)
import CSS.Common (center)
import CSS.Flexbox (alignItems, column, row)
import Data.Array as Array
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.String (drop)
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (class MonadEffect)
import Effect.Class.Console (log)
import Examples.Halogen.Canvas.Sketch as Sketch
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
import Web.HTML (window)
import Web.HTML.Location (hash, setHash)
import Web.HTML.Window (location)

type Example =
  { id :: String
  , name :: String
  , component :: H.Component Maybe Unit Unit Aff
  }

type Examples = Array Example


main :: Effect Unit
main = do
  HA.runHalogenAff do
    body <- HA.awaitBody
    let examples =
          [ { id: "xterm"
            , name: "XTerm"
            , component: XTerm.component
            }
          , { id: "infinite-scroll"
            , name: "Infinite Scroll"
            , component: InfiniteScroll.feedComponent
            }
          , { id: "halogen-canvas-sketch"
            , name: "Line Sketch"
            , component: Sketch.component
            }
          ]
    void $ runUI component ("xterm" /\ examples) body

type Slots = ( example :: H.Slot Maybe Unit String ) 

_example = Proxy :: Proxy "example"

type State =
  { selected :: String 
  , examples :: Map String Example
  }
  


component :: forall q o . H.Component q (String /\ Examples) o Aff
component =
  H.mkComponent
    { initialState: \(selected /\ e) -> { selected, examples: Map.fromFoldable ((\x -> x.id /\ x ) <$> e) } 
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction
                                     , initialize = Just Initialize
                                     }
    }


data Action =
    Initialize
  | SelectExample String 

selectExample :: Event -> Action
selectExample e = maybe (SelectExample "") SelectExample $ (\x -> (unsafeCoerce x).value) <$> target e 

handleAction :: forall o m. MonadEffect m => Action -> H.HalogenM State Action Slots o m Unit 
handleAction =
  case _ of
    Initialize -> do
       h <- H.liftEffect $ window >>= location >>= hash
       H.liftEffect $ log $ drop 1 h 
       H.modify_ $ \st ->
         case Map.lookup (drop 1 h) st.examples of
           Nothing -> st
           Just so -> st { selected = so.id }
       pure unit
    SelectExample e -> do
      H.liftEffect $ window >>= location >>= setHash e
      H.modify_ (\st -> st { selected = e }) 


selector :: State -> H.ComponentHTML Action Slots Aff 
selector st = 
  HH.select
    [ HP.value st.selected 
    , HE.onChange selectExample
    ]
    (mkOption <$> Array.fromFoldable (Map.values st.examples))
    where
      mkOption v =
        HH.option
          [ HP.value v.id
          ]
          [ HH.text v.name
          ]


render :: State -> H.ComponentHTML Action Slots Aff 
render st =
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
        [ HH.h3_ [ HH.text "Example" ]
        , HH.div [ style $ padding (px 0.0) (px 0.0) (px 0.0) (px 10.0)] [ selector st ]
        ]
    , HH.div
        [ style do
            border solid (px 1.0) black
        ]
        [ case Map.lookup st.selected st.examples of
            Just v -> HH.slot_ _example v.id v.component unit
            Nothing -> HH.div_ []
        ]
    ]
 
