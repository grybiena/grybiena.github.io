module Example.Button where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Web.Event.Event (Event, stopPropagation)
import Web.UIEvent.MouseEvent (toEvent)

type Slots :: forall k. Row k
type Slots = ()

data Action = Click Event

data Query a = SetText String a

component :: forall m. MonadAff m => H.Component Query String Unit m
component = do
  H.mkComponent
    { initialState: identity 
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction
                                     , handleQuery = handleQuery
                                     }
    }

render :: forall m. MonadAff m => String -> H.ComponentHTML Action Slots m
render s = HH.button [ HE.onClick (Click <<< toEvent)] [HH.text s] 

handleAction :: forall m .
                MonadAff m
             => Action
             -> H.HalogenM String Action Slots Unit m Unit
handleAction = case _ of
  Click e -> do
    H.liftEffect $ stopPropagation e
    H.raise unit 

handleQuery :: forall m a .
                MonadAff m
             => Query a
             -> H.HalogenM String Action Slots Unit m (Maybe a) 
handleQuery = case _ of
  SetText txt a -> do
    H.put txt
    pure (Just a)

