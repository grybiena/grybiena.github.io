module Example.Button where

import Prelude

import Data.Maybe (Maybe(..))
import Data.String (joinWith)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.Shell.Free (Args, Stdin(..), Stdout(..), terminal)
import Halogen.Terminal.Free (write)
import Web.Event.Event (Event, stopPropagation)
import Web.UIEvent.MouseEvent (toEvent)

type Slots :: forall k. Row k
type Slots = ()

data Action = Click Event

component :: forall s o m. MonadAff m => H.Component Stdin Args (Stdout s o m) m
component = do
  H.mkComponent
    { initialState: joinWith " "
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction
                                     , handleQuery = handleQuery
                                     }
    }

render :: forall m. MonadAff m => String -> H.ComponentHTML Action Slots m
render s = HH.button [ HE.onClick (Click <<< toEvent)] [HH.text s] 

handleAction :: forall s o m .
                MonadAff m
             => Action
             -> H.HalogenM String Action Slots (Stdout s o m) m Unit
handleAction = case _ of
  Click e -> do
    H.liftEffect $ stopPropagation e
    H.raise $ Stdout (terminal $ write "click!\r\n")


handleQuery :: forall s o m a .
                MonadAff m
             => Stdin a
             -> H.HalogenM String Action Slots (Stdout s o m) m (Maybe a)
handleQuery = case _ of
  Stdin txt e -> do
    H.put txt
    pure (Just e)


