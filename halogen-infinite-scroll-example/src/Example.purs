module Example where

import CSS (borderColor, display, flex, flexDirection, height, left, row, top, width)
import CSS.Size (px)
import Color (black)
import Control.Alt (void)
import Control.Category (identity, (<<<))
import DOM.HTML.Indexed.InputType (InputType(..))
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse_)
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (class MonadEffect)
import Effect.Timer (IntervalId, clearInterval, setInterval)
import Example.Picture (PicIndex(..))
import Halogen (PropName(..), SubscriptionId)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.CSS (style)
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Infinite.Scroll (Query(..), defaultFeedOptions)
import Halogen.Infinite.Scroll as HIS
import Halogen.Shell as Shell
import Halogen.Shell.Free (terminal)
import Halogen.Subscription as HS
import Halogen.Terminal.Free (loadAddons, writeLn)
import Halogen.VDom.Driver (runUI)
import Prelude (Unit, bind, const, discard, flip, not, show, unit, ($))
import Type.Proxy (Proxy(..))


main :: Effect Unit
main = do
  HA.runHalogenAff do
    body <- HA.awaitBody
    void $ runUI feedComponent unit body

type Slots = ( feed :: forall o. H.Slot (HIS.Query PicIndex) o Boolean
             , shell :: forall o. H.Slot (Shell.Query String Unit) o Unit
             ) 

_feed = Proxy :: Proxy "feed"
_shell = Proxy :: Proxy "shell"

type State =
  { top :: Boolean
  , log :: Maybe (SubscriptionId /\ IntervalId) 
  }
  

feedComponent :: forall q o . H.Component q Unit o Aff
feedComponent =
  H.mkComponent
    { initialState: const { top: false, log: Nothing } 
    , render: renderExample
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }


data Action =
    ToggleTop 
  | ToggleLog
  | LogTop

logShell :: String -> Shell.Query String Unit Unit
logShell s = Shell.Query s identity


handleAction :: forall o m. MonadEffect m => Action -> H.HalogenM State Action Slots o m Unit 
handleAction =
  case _ of
      ToggleTop -> do
        s <- H.modify (\st -> st { top = not st.top })
        let msg = if s.top then "top loading/unloading enabled" else "top loading/unloading disabled"
        void $ H.query _shell unit (logShell msg) 
      ToggleLog -> do
         l <- H.gets (\st -> st.log)
         case l of
           Just (s /\ i) -> do
             H.modify_ (\st -> st { log = Nothing })
             H.unsubscribe s 
             H.liftEffect $ clearInterval i
           Nothing -> do
              { listener, emitter } <- H.liftEffect $ HS.create
              i <- H.liftEffect $ setInterval 1000 (HS.notify listener LogTop)
              s <- H.subscribe emitter
              H.modify_ (\st -> st { log = Just (s /\ i) })
      LogTop -> do
         t <- H.gets (\st -> st.top)
         o <- H.query _feed t (GetScrollTop identity) 
         flip traverse_ o $ \g -> do
           void $ H.query _shell unit (logShell (show g))



renderExample :: State -> H.ComponentHTML Action Slots Aff 
renderExample t =
  HH.div
    [ style do
        display flex
        flexDirection row
    ]
    [ HH.div_
        [ HH.div_
            [ HH.input [ HP.prop (PropName "type") InputCheckbox
                       , HE.onChange (const ToggleTop)
                       ]
            , HH.text "enable top loading/unloading"
            ]
        , HH.div_
            [ HH.input [ HP.prop (PropName "type") InputCheckbox
                       , HE.onChange (const ToggleLog)
                       ]
            , HH.text "enable scrollTop logging"
            ]
        ]
    , HH.div [ style do
                 top (px 10.0)
                 left (px 10.0)
                 width (px 400.0)
                 height (px 1200.0)
                 borderColor black
             ]
             [ HH.slot_ _feed t.top HIS.component ((defaultFeedOptions (PicIndex 150)) { enableTop = t.top })
             ]
    , HH.slot_ _shell unit Shell.component shell
    ]
  where
    shell =
      { init: do
          terminal do
            loadAddons false 
      , query: terminal <<< writeLn
      , shell: unit 
      }
    
    
