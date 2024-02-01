module Examples.Halogen.Canvas.Sketch where

import Prelude hiding (top)

import Color (rgb)
import Control.Monad.Rec.Class (class MonadRec)
import Data.Array (cons, (:))
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), maybe)
import Data.Traversable (traverse_)
import Data.Tuple.Nested (type (/\), (/\))
import Effect.Aff.Class (class MonadAff)
import Graphics.Canvas.Free (Coordinate, beginPath, fillRect, lineTo, moveTo, setFillColor, setLineWidth, setStrokeColor, stroke)
import Halogen as H
import Halogen.Canvas.Interact (InputEvent(..), Interact)
import Halogen.Canvas.Interact as Interact
import Halogen.HTML as HH
import Type.Prelude (Proxy(..))
import Web.UIEvent.MouseEvent (clientY,clientX)


sketch :: forall m. Interact { line :: Maybe (Coordinate /\ Coordinate), lines :: Array (Coordinate /\ Coordinate) } m 
sketch = { dimensions: { width: 400, height: 400 }
       , world: { line: Nothing, lines: [] }
       , draw: \w -> do
           setFillColor $ rgb 255 255 200
           fillRect { x: 0.0, y: 0.0, width: 400.0, height: 400.0 }
           setStrokeColor $ rgb 0 0 0
           setLineWidth 5.0
           flip traverse_ (maybe w.lines (flip cons w.lines) w.line) $ \(f /\ t) -> do
              beginPath
              moveTo f
              lineTo t                       
              stroke
       , input: \i r w ->
           case i of
             MouseLeave _ -> w { line = Nothing }
             MouseUp _ ->
               w { line = Nothing
                 , lines =
                     case w.line of
                       Nothing -> w.lines
                       Just so -> so:w.lines
                 }
             MouseDown e ->
               let p = { x: toNumber (clientX e) - r.left, y: toNumber (clientY e) - r.top }
                in  w { line = maybe (Just (p /\ p)) Just w.line }
             MouseMove e ->
               let p = { x: toNumber (clientX e) - r.left, y: toNumber (clientY e) - r.top }
                   update (f /\ _) = (f /\ p)
                in w { line = update <$> w.line }
             _ -> w
       }

type Slots = ( sketch :: forall q o. H.Slot q o Unit ) 

_sketch = Proxy :: Proxy "sketch"

component :: forall q i o m . MonadAff m => MonadRec m => H.Component q i o m 
component =
  H.mkComponent
    { initialState: const unit 
    , render: const $ HH.slot_ _sketch unit Interact.component sketch
    , eval: H.mkEval $ H.defaultEval 
    }



