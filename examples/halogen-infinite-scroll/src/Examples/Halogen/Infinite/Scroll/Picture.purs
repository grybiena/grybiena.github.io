module Examples.Halogen.Infinite.Scroll.Picture where

import CSS (border, height, solid, width)
import CSS.Size (px)
import Color (black)
import Control.Monad.Rec.Class (Step(..), tailRecM)
import Data.EuclideanRing (mod)
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS (style)
import Halogen.HTML.Properties as HP
import Halogen.Infinite.Scroll (class Feed)
import Halogen.Infinite.Scroll.Page (class PageElement, class PageOrder)
import Pipes (yield)
import Prelude (class Eq, class Ord, class Show, compare, discard, eq, identity, pure, show, unit, ($), (+), (-), (<>))


newtype PicIndex = PicIndex Int

instance Show PicIndex where 
  show (PicIndex a) = show a

instance Eq PicIndex where
  eq (PicIndex a) (PicIndex b) = eq a b

instance Ord PicIndex where
  compare (PicIndex a) (PicIndex b) = compare a b

instance PageOrder PicIndex where
  pageOrder (PicIndex a) (PicIndex b) = compare a b

instance MonadAff m => Feed PicIndex m where
  feedAbove (PicIndex i) = do
    pure (tailRecM go i)
    where go x = do
            let z = (x-1) `mod` 300
            yield (PicIndex z)
            pure $ Loop z
  feedBelow (PicIndex i) = do
    pure (tailRecM go i)
    where go x = do
            let z = (x+1) `mod` 300
            yield (PicIndex z)
            pure $ Loop z
  feedInsert = pure Nothing
  feedDelete = pure Nothing
  onElement _ = pure unit

instance MonadAff m => PageElement PicIndex m where
  element =
      H.mkComponent
        { initialState: identity 
        , render: renderPic
        , eval: H.mkEval H.defaultEval
        }
      where
        renderPic (PicIndex i) = HH.div [ style do
                                                   border solid (px 1.0) black
                                               ] [ HH.text (show i)
                                                 , HH.img [HP.src ("https://picsum.photos/" <> show i)
                                                          ,style do
                                                             width (px 400.0)
                                                             height (px 400.0)
                                                          ]
                                                 ]


