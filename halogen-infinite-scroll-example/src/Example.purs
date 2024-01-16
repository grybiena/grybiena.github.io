module Example where

import CSS (border, borderColor, height, left, solid, top, width)
import CSS.Size (px)
import Color (black)
import Control.Alt (void)
import Control.Monad.Rec.Class (Step(..), tailRecM)
import Data.EuclideanRing (mod)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.CSS (style)
import Halogen.HTML.Properties as HP
import Halogen.Infinite.Scroll (class Feed, class FeedOrder)
import Halogen.Infinite.Scroll as HIS
import Halogen.VDom.Driver (runUI)
import Pipes (yield)
import Prelude (class Eq, class Ord, class Show, Unit, bind, compare, const, discard, eq, identity, pure, show, unit, ($), (+), (-), (<>))
import Type.Proxy (Proxy(..))


main :: Effect Unit
main = do
  HA.runHalogenAff do
    body <- HA.awaitBody
    void $ runUI exampleComponent unit body

type DemoSlots = ( example :: forall q . H.Slot q HIS.ScrollFeed Unit ) 

_example = Proxy :: Proxy "example"


exampleComponent :: forall q o . H.Component q Unit o Aff
exampleComponent =
  H.mkComponent
    { initialState: const unit 
    , render: renderExample
    , eval: H.mkEval H.defaultEval 
    }

scrollHeight :: Number
scrollHeight = 1200.0

renderExample :: forall a. Unit -> H.ComponentHTML a DemoSlots Aff 
renderExample _ =
  HH.div_
    [ HH.h3_ [ HH.a [ HP.href "https://github.com/grybiena/halogen-infinite-scroll" ] [ HH.text "halogen-infinite-scroll" ]
             ]
    , HH.div [ style do
                 top (px 10.0)
                 left (px 10.0)
                 width (px 400.0)
                 height (px scrollHeight)
                 borderColor black
             ]
             [ HH.slot_ _example unit HIS.component (HIS.defaultFeedParams (PicIndex 150))
             ]
    , HH.div_
      [ HH.h3_ [ HH.text "About" ]
      , HH.p_ [ HH.text "The components in this example load pictures from "
              , HH.a [ HP.href "https://picsum.photos/" ] [HH.text "this random image API."]
              ]
      , HH.p_ [ HH.text "This library generally works very smoothly on mobile browsers. Scrolling can be jerky on PC."
              , HH.text "If you are browsing on PC and experiencing jerk while scrolling try opening a mobile device emulator in developer tools."
              ]

      ]
    ]

newtype PicIndex = PicIndex Int

instance Show PicIndex where 
  show (PicIndex a) = show a

instance Eq PicIndex where
  eq (PicIndex a) (PicIndex b) = eq a b

instance Ord PicIndex where
  compare (PicIndex a) (PicIndex b) = compare a b

instance FeedOrder PicIndex where
  feedOrder (PicIndex a) (PicIndex b) = compare a b

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

