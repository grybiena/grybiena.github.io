module Example where

import CSS (borderColor, display, flex, flexDirection, height, left, row, top, width)
import CSS.Size (px)
import Color (black)
import Control.Alt (void)
import Control.Category (identity, (<<<))
import DOM.HTML.Indexed.InputType (InputType(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Example.Picture (PicIndex(..))
import Halogen (PropName(..))
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.CSS (style)
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Infinite.Scroll (defaultFeedOptions)
import Halogen.Infinite.Scroll as HIS
import Halogen.Shell as Shell
import Halogen.Shell.Free (terminal)
import Halogen.Terminal.Free (loadAddons, writeLn)
import Halogen.VDom.Driver (runUI)
import Prelude (Unit, bind, const, discard, not, unit, ($))
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


feedComponent :: forall q o . H.Component q Unit o Aff
feedComponent =
  H.mkComponent
    { initialState: const false 
    , render: renderExample
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }


data Action = Toggle 

handleAction :: forall o m. Action -> H.HalogenM Boolean Action Slots o m Unit 
handleAction =
  case _ of
      Toggle -> do
        let q :: String -> Shell.Query String Unit Unit
            q s = Shell.Query s identity
        H.modify_ not
        s <- H.get
        void $ H.query _shell unit (q if s then "top loading/unloading enabled" else "top loading/unloading disabled") 



renderExample :: Boolean -> H.ComponentHTML Action Slots Aff 
renderExample t =
  HH.div
    [ style do
        display flex
        flexDirection row
    ]
    [ HH.div_
        [ HH.input [ HP.prop (PropName "type") InputCheckbox
                   , HE.onChange (const Toggle)
                   ]
        , HH.text "enable top loading/unloading"
        ]
    , HH.div [ style do
                 top (px 10.0)
                 left (px 10.0)
                 width (px 400.0)
                 height (px 1200.0)
                 borderColor black
             ]
             [ HH.slot_ _feed t HIS.component ((defaultFeedOptions (PicIndex 150)) { enableTop = t })
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
    
    
