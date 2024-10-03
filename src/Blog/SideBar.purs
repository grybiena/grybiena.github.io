module Blog.SideBar where

import Prelude

import Affjax.ResponseFormat (json)
import Affjax.Web (get)
import Blog.Post (BlogPost(..))
import CSS (alignItems, column, display, em, flex, flexDirection, fontSize, height, margin, padding, pt, px, width)
import CSS.Common (center)
import Data.Argonaut (class DecodeJson, decodeJson)
import Data.Either (either)
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS (style)
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

type State =
  { posts :: Array BlogPost
  , links :: Array Link
  }

component :: forall q . H.Component q (Array BlogPost) BlogPost Aff
component =
  H.mkComponent
    { initialState: \posts -> { posts, links: [] }
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction
                                     , initialize = Just Initialize
                                     }
    }

data Action =
    Initialize
  | GoToPost BlogPost

handleAction :: Action -> H.HalogenM State Action Slots BlogPost Aff Unit
handleAction Initialize = do
  links <- H.liftAff getLinks
  H.modify_ (\st -> st { links = links })
handleAction (GoToPost post) = H.raise post 

type Slots :: forall k. Row k
type Slots = ()

render :: State -> H.ComponentHTML Action Slots Aff
render { posts, links } = HH.div
  [ style do
      padding (em 0.0) (em 1.0) (em 0.0) (em 1.0)
  ]
  [ HH.h4_ [ HH.text "blog.grybiena.com" ]
  , profilePicture 
  , blurb
  , renderLinks links
  , HH.h4 [] [ HH.text "Posts" ]
  , recentPosts posts 
  ]


recentPosts :: forall m. Array BlogPost -> HH.HTML m Action
recentPosts content =
  HH.div_ $ renderPostLink <$> content
  where
    renderPostLink bp@(BlogPost { title, post }) =
      HH.div_
        [ HH.a [ HP.href ("#" <> post)
               , HE.onClick (const $ GoToPost bp)
               ] [ HH.h4_ [ HH.text title ] ]
        ]


blurb :: forall a b. HH.HTML a b 
blurb =
  HH.div
    [
    ]
    [ HH.p_
        [ HH.text "blog.grybiena.com is a blog about software engineering with a focus on distributed systems, mesh networking, and protocol design. "
        , HH.text "Other topics covered by this blog include functional programming, programming language design, dev-ops, reproducable build systems, and the art of programming. "
        ]
    ]

profilePicture :: forall a b. HH.HTML a b 
profilePicture =
  HH.div
    [ style do
        display flex
        flexDirection column
        alignItems center
    ]
    [ HH.img
        [ style do
            width (px 200.0)
            height (px 200.0)
        , HP.src "https://avatars.githubusercontent.com/u/135348827?v=4"
        ] 
    ]

newtype Link =
  Link {
    name :: String
  , href :: String
  , text :: String
  }

derive newtype instance DecodeJson Link

getLinks :: Aff (Array Link)
getLinks = either (const []) (\r -> either (const []) identity (decodeJson r.body)) <$> get json "/links.json"

renderLinks :: forall a b. Array Link -> HH.HTML a b
renderLinks links = HH.div_ (renderLink <$> links)

renderLink :: forall a b. Link -> HH.HTML a b 
renderLink (Link { name, href, text }) =
  HH.div
    [
    ]
    [ HH.p
        [ style do
            fontSize (pt 10.0) 
            margin (px 0.0) (px 0.0) (px 0.0) (px 10.0)
        ]
        [ HH.text name ]
    , HH.p
        [ style do
            margin (px 0.0) (px 0.0) (px 0.0) (px 10.0)
        ]
        [ HH.a
            [ HP.href href
            ]
            [ HH.text text
            ]
        ]
    ]

