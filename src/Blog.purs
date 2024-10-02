module Blog where

import Prelude hiding (top)

import Blog.Header as Header
import Blog.Post (BlogPost(..), getPostList)
import Blog.Post as BlogPost
import Blog.SideBar as SideBar
import CSS (absolute, backgroundColor, column, display, flex, flexDirection, flexGrow, fontFamily, fromHexString, left, margin, maxHeight, minHeight, padding, pct, position, px, relative, row, top, width)
import CSS.Font (monospace)
import Code.Highlight (highlightAll)
import Color as Color
import Control.Monad.State (class MonadState)
import Data.Array (find, head, length)
import Data.Maybe (Maybe(..), maybe)
import Data.NonEmpty (singleton)
import Data.String (drop)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.CSS (style)
import Halogen.VDom.Driver (runUI)
import Type.Proxy (Proxy(..))
import Web.HTML (window)
import Web.HTML.Location (hash, setHash)
import Web.HTML.Window (location)


main :: Effect Unit
main = do
  HA.runHalogenAff do
     body <- HA.awaitBody
     void $ runUI component unit body

type State =
  { content :: Array BlogPost
  , selected :: Maybe BlogPost
  }

component :: forall q i o . H.Component q i o Aff
component =
  H.mkComponent
    { initialState: const { content: [], selected: Nothing } 
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction
                                     , initialize = Just Initialize
                                     }
    }

data Action =
    Initialize
  | SelectPost BlogPost

handleAction :: forall m. MonadAff m => MonadState State m => Action -> m Unit 
handleAction Initialize = do
  content <- H.liftAff getPostList
  h <- drop 1 <$> (liftEffect $ window >>= location >>= hash)
  H.modify_ (\s -> s { content = content, selected = find (\(BlogPost { post }) -> post == h) content })
  liftEffect highlightAll
handleAction (SelectPost content@(BlogPost { post })) = do
  liftEffect $ window >>= location >>= setHash post
  H.modify_ (\s -> s { selected = Just content})
  liftEffect highlightAll


type Slots = ( header :: forall m . H.Slot m Unit Unit
             , sidebar :: forall m . H.Slot m BlogPost Int 
             , content :: forall m . H.Slot m Unit String 
             )

_header = Proxy :: Proxy "header"
_sidebar = Proxy :: Proxy "sidebar"
_content = Proxy :: Proxy "content"

render :: State -> H.ComponentHTML Action Slots Aff
render { content, selected } = HH.div
  [ style do
          position absolute 
          display flex
          flexDirection column
          minHeight (pct 100.0)
          margin (px 0.0) (px 0.0) (px 0.0) (px 0.0)
          padding (px 0.0) (px 0.0) (px 0.0) (px 0.0)
          left (px 0.0)
          top (px 0.0)
          fontFamily [] (singleton monospace) 
  ]
  [ HH.slot_ _header unit Header.component unit
  , HH.div
      [ style do
          position relative
          display flex
          flexDirection row
          flexGrow 4.0
          maxHeight (pct 100.0)
      ]
      [ HH.div
          [ style do
              flexGrow 4.0
              width (pct 70.0)
              backgroundColor (maybe Color.white identity (fromHexString "#D6EAF8")) 
          ]
          [ case selected of
              Nothing ->
                case head content of
                   Nothing -> HH.div_ []
                   Just p@(BlogPost { post }) -> HH.slot_ _content post BlogPost.component p
              Just p@(BlogPost { post }) -> HH.slot_ _content post BlogPost.component p 
          ]
      , HH.div
          [ style do
              flexGrow 1.0
              width (pct 30.0)
              backgroundColor (maybe Color.white identity (fromHexString "#D2B4DE")) 
          ]
          [ HH.slot _sidebar (length content) SideBar.component content SelectPost
          ]
      ]
  ]

