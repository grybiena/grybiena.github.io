module Blog.SideBar where

import Prelude

import Blog.Post (BlogPost(..))
import CSS (alignItems, column, display, em, flex, flexDirection, fontSize, height, margin, padding, pt, px, width)
import CSS.Common (center)
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS (style)
import Halogen.HTML.Properties as HP
import Halogen.HTML.Events as HE

component :: forall q . H.Component q (Array BlogPost) BlogPost Aff
component =
  H.mkComponent
    { initialState: identity 
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction
                                     }
    }

data Action = GoToPost BlogPost

handleAction :: Action -> H.HalogenM (Array BlogPost) Action Slots BlogPost Aff Unit
handleAction (GoToPost post) = H.raise post 

type Slots :: forall k. Row k
type Slots = ()

render :: Array BlogPost -> H.ComponentHTML Action Slots Aff
render content = HH.div
  [ style do
      padding (em 0.0) (em 1.0) (em 0.0) (em 1.0)
  ]
  [ HH.h4_ [ HH.text "blog.grybiena.com" ]
  , profilePicture 
  , blurb
  , githubLink
  , emailLink
  , HH.h4 [] [ HH.text "Posts" ]
  , recentPosts content
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

githubLink :: forall a b. HH.HTML a b 
githubLink =
  HH.div
    [
    ]
    [ HH.p
        [ style do
            fontSize (pt 10.0) 
            margin (px 0.0) (px 0.0) (px 0.0) (px 10.0)
        ]
        [ HH.text "github" ]
    , HH.p
        [ style do
            margin (px 0.0) (px 0.0) (px 0.0) (px 10.0)
        ]
        [ HH.a
            [ HP.href "https://github.com/grybiena"
            ]
            [ HH.text "github.com/grybiena"
            ]
        ]
    ]


emailLink :: forall a b. HH.HTML a b 
emailLink =
  HH.div
    [
    ]
    [ HH.p
        [ style do
            fontSize (pt 10.0) 
            margin (px 0.0) (px 0.0) (px 0.0) (px 10.0)
        ]
        [ HH.text "email" ]
    , HH.p
        [ style do
            margin (px 0.0) (px 0.0) (px 0.0) (px 10.0)
        ]
        [ HH.a
            [ HP.href "fraser@grybiena.com"
            ]
            [ HH.text "fraser@grybiena.com"
            ]
        ]
    ]

