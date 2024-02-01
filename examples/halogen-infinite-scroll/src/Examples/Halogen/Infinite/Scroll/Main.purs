module Examples.Halogen.Infinite.Scroll.Main where

import Prelude hiding (top)

import CSS (borderColor, display, flex, flexDirection, height, left, row, top, width)
import CSS.Size (px)
import Color (black)
import DOM.HTML.Indexed.InputType (InputType(..))
import Data.Array as Array
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (traverse_)
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (class MonadEffect)
import Effect.Timer (IntervalId, clearInterval, setInterval)
import Examples.Halogen.Infinite.Scroll.Picture (PicIndex(..))
import Halogen (PropName(..), SubscriptionId)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.CSS (style)
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Infinite.Scroll (Query(..), defaultFeedOptions)
import Halogen.Infinite.Scroll as HIS
import Halogen.Infinite.Scroll.State (FeedState)
import Halogen.Subscription as HS
import Halogen.VDom.Driver (runUI)
import Halogen.XShell as Shell
import Halogen.XShell.Free (ShellM, terminal)
import Halogen.XTerm.Free (writeLn)
import Type.Proxy (Proxy(..))


main :: Effect Unit
main = do
  HA.runHalogenAff do
    body <- HA.awaitBody
    void $ runUI feedComponent unit body

type Slots = ( feed :: H.Slot (HIS.Query PicIndex) (FeedState PicIndex) Boolean
             , shell :: forall w o. H.Slot (ShellM w Unit o Aff) o Unit
             ) 

_feed = Proxy :: Proxy "feed"
_shell = Proxy :: Proxy "shell"

type State =
  { top :: Boolean
  , up :: Boolean
  , log :: Maybe (SubscriptionId /\ IntervalId) 
  }
  

feedComponent :: forall q o . H.Component q Unit o Aff
feedComponent =
  H.mkComponent
    { initialState: const { top: false, up: false, log: Nothing } 
    , render: renderExample
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }


data Action =
    ToggleTop 
  | ToggleScroll
  | ToggleUpDn
  | AutoScroll
  | StateChange (FeedState PicIndex)


logShell :: forall w s o m . String -> ShellM w s o m Unit
logShell s = terminal $ writeLn s 


handleAction :: forall o m. MonadEffect m => Action -> H.HalogenM State Action Slots o m Unit 
handleAction =
  case _ of
      ToggleTop -> do
        s <- H.modify (\st -> st { top = not st.top })
        let msg = if s.top then "top loading/unloading enabled" else "top loading/unloading disabled"
        void $ H.query _shell unit (logShell msg) 
      ToggleScroll -> do
         l <- H.gets (\st -> st.log)
         case l of
           Just (s /\ i) -> do
             H.modify_ (\st -> st { log = Nothing })
             H.unsubscribe s 
             H.liftEffect $ clearInterval i
           Nothing -> do
              { listener, emitter } <- H.liftEffect $ HS.create
              i <- H.liftEffect $ setInterval 200 (HS.notify listener AutoScroll)
              s <- H.subscribe emitter
              H.modify_ (\st -> st { log = Just (s /\ i) })
      ToggleUpDn -> H.modify_ (\st -> st { up = not st.up })
      AutoScroll -> do
         t <- H.gets (\st -> st.top)
         o <- H.query _feed t (GetScrollTop identity) 
         up <- H.gets (\st -> st.up)
         flip traverse_ o $ \g -> do
           let amount = if up then -99.0 else 99.0
           void $ H.query _feed t (ScrollFeed amount unit)
           u <- H.query _feed t (GetScrollTop identity) 
           flip traverse_ u $ \f -> do
             void $ H.query _shell unit (logShell ("scroll: " <> show g <> "+=" <> show amount))
             when (f /= g + amount) do
               void $ H.query _shell unit (logShell $ "scroll offset error" <> show f)
               handleAction ToggleScroll
      StateChange { pages, update } -> do
         void $ H.query _shell unit (logShell ("pages: " <> (show $ Array.fromFoldable $ Map.keys pages)))
         void $ H.query _shell unit (logShell ("adjust: " <> (show update.scroll)))




renderExample :: State -> H.ComponentHTML Action Slots Aff 
renderExample t =
  HH.div
    [ style do
        display flex
        flexDirection row
    ]
    [ HH.div [ style do
                 top (px 10.0)
                 left (px 10.0)
                 width (px 400.0)
                 height (px 1200.0)
                 borderColor black
             ]
             [ HH.slot _feed t.top HIS.component ((defaultFeedOptions (PicIndex 150)) { enableTop = t.top, debounce = Milliseconds 0.0, deadZone = 1 }) StateChange
             ]
    , HH.div_
        [ HH.slot_ _shell unit Shell.component unit 
        , HH.div_
            [ HH.input [ HP.prop (PropName "type") InputCheckbox
                       , HE.onChange (const ToggleTop)
                       ]
            , HH.text "enable top loading/unloading"
            ]
        , HH.div_
            [ HH.input [ HP.prop (PropName "type") InputCheckbox
                       , HE.onChange (const ToggleScroll)
                       ]
            , HH.text "enable automatic scrolling"
            ]
        , HH.div_
            [ HH.input [ HP.prop (PropName "type") InputCheckbox
                       , HE.onChange (const ToggleUpDn)
                       ]
            , HH.text "auto scroll up"
            ]
        ]
    ]
   
