module Examples.Halogen.Canvas.Sketch where

import Prelude hiding (top)

import CSS (display, flex, flexDirection)
import CSS.Flexbox (row)
import Color (rgb)
import Control.Monad.Rec.Class (class MonadRec)
import Data.Array (cons, length, zip, (..), (:))
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), maybe)
import Data.Traversable (traverse_)
import Data.Tuple.Nested (type (/\), (/\))
import Effect.Aff.Class (class MonadAff)
import Graphics.Canvas.Free (CanvasT, Coordinate, beginPath, clearRect, fillRect, getHeight, getWidth, lineTo, moveTo, setFillColor, setLineWidth, setStrokeColor, stroke, withContext)
import Halogen as H
import Halogen.Canvas (Dimensions)
import Halogen.Canvas.Interact (MouseInput(..), Output(..))
import Halogen.Canvas.Interact as Interact
import Halogen.HTML as HH
import Halogen.HTML.CSS (style)
import Type.Prelude (Proxy(..))
import Web.UIEvent.MouseEvent (clientY, clientX)

type Slots m = ( sketch :: H.Slot (CanvasT m) Interact.Output Unit ) 

_sketch = Proxy :: Proxy "sketch"

type Line = Coordinate /\ Coordinate

data Action = InputEvent Interact.Output

type State =
  { dimensions :: Dimensions
  , line :: Maybe Line
  , lines :: Array Line
  }

component :: forall q i o m . MonadAff m => MonadRec m => H.Component q i o m 
component =
  H.mkComponent
    { initialState: const { dimensions: { width: 400, height: 400 }, line: Nothing, lines: [] } 
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction } 
    }

render :: forall m. MonadAff m => MonadRec m => State -> H.ComponentHTML Action (Slots m) m 
render { dimensions, lines } =
  HH.div
    [ style do
        display flex
        flexDirection row
    ]
    [ HH.slot _sketch unit Interact.component dimensions InputEvent 
    , HH.div_
        [ HH.h3_ [ HH.text "Lines" ]
        , renderLines lines
        ]
    ]

renderLines :: forall m. MonadAff m => MonadRec m => Array Line -> H.ComponentHTML Action (Slots m) m 
renderLines lines = HH.div_ $ renderLine <$> (zip (0 .. (length lines)) lines)
  where
    renderLine (idx /\ start /\ end) =
      HH.div_ [ HH.text $ show idx <> " " <> show start <> " " <> show end ]


handleAction :: forall o m. Action -> H.HalogenM State Action (Slots m) o m Unit
handleAction =
  case _ of
    InputEvent i -> do
      s <- interact i
      void $ H.query _sketch unit do
         withContext do
           width <- getWidth
           height <- getHeight
           clearRect { x: 0.0, y: 0.0, width, height }
           draw s


draw :: forall m. State -> CanvasT m Unit
draw w = do
  setFillColor $ rgb 255 255 200
  fillRect { x: 0.0, y: 0.0, width: 400.0, height: 400.0 }
  setStrokeColor $ rgb 0 0 0
  setLineWidth 5.0
  flip traverse_ (maybe w.lines (flip cons w.lines) w.line) $ \(f /\ t) -> do
     beginPath
     moveTo f
     lineTo t                       
     stroke

interact :: forall o m. Interact.Output -> H.HalogenM State Action (Slots m) o m State
interact =
  case _ of
    KeyEvent _ -> H.get 
    MouseEvent i r ->
      case i of
        MouseLeave _ -> H.modify (\w -> w { line = Nothing })
        MouseUp _ ->
          H.modify $ \w ->
            w { line = Nothing
              , lines =
                  case w.line of
                    Nothing -> w.lines
                    Just so -> so:w.lines
              }
        MouseDown e -> do
          let p = { x: toNumber (clientX e) - r.left, y: toNumber (clientY e) - r.top }
          H.modify $ \w -> w { line = maybe (Just (p /\ p)) Just w.line }
        MouseMove e -> do
          let p = { x: toNumber (clientX e) - r.left, y: toNumber (clientY e) - r.top }
              update (f /\ _) = (f /\ p)
          H.modify $ \w -> w { line = update <$> w.line }
        _ -> H.get 

