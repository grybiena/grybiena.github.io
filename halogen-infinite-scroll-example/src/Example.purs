module Example where

import CSS (border, borderColor, height, left, solid, top, width)
import CSS.Size (px)
import Color (black)
import Control.Alt (void)
import Control.Monad.Rec.Class (Step(..), tailRecM)
import DOM.HTML.Indexed.InputType (InputType(..))
import Data.EuclideanRing (mod)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Halogen (PropName(..))
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.CSS (style)
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Infinite.Scroll (class Feed, class FeedOrder)
import Halogen.Infinite.Scroll as HIS
import Halogen.VDom.Driver (runUI)
import Pipes (yield)
import Prelude (class Eq, class Ord, class Show, Unit, bind, compare, const, discard, eq, identity, not, pure, show, unit, ($), (+), (-), (<>))
import Type.Proxy (Proxy(..))


main :: Effect Unit
main = do
  HA.runHalogenAff do
    body <- HA.awaitBody
    void $ runUI exampleComponent unit body

type DemoSlots = ( example :: forall q . H.Slot q HIS.ScrollFeed Boolean ) 

_example = Proxy :: Proxy "example"


exampleComponent :: forall q o . H.Component q Unit o Aff
exampleComponent =
  H.mkComponent
    { initialState: const false 
    , render: renderExample
    , eval: H.mkEval $ H.defaultEval { handleAction = \Toggle -> H.modify_ not }
    }

scrollHeight :: Number
scrollHeight = 1200.0

data Action = Toggle 

renderExample :: Boolean -> H.ComponentHTML Action DemoSlots Aff 
renderExample t =
  HH.div_
    [ HH.h3_ [ HH.a [ HP.href "https://github.com/grybiena/halogen-infinite-scroll" ] [ HH.text "halogen-infinite-scroll" ]
             ]
    , HH.div_
        [ HH.input [ HP.prop (PropName "type") InputCheckbox
                   , HE.onChange (const Toggle)
                   ]
        , HH.text "enable top loading/unloading"
        ]
    , HH.div [ style do
                 top (px 10.0)
                 left (px 10.0)
                 width (px 400.0)
                 height (px scrollHeight)
                 borderColor black
             ]
             [ HH.slot_ _example t HIS.component ((HIS.defaultFeedParams (PicIndex 150)) { enableTop = t })
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

