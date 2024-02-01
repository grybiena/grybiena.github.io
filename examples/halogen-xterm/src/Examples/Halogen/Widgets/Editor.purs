module Examples.Halogen.Widgets.Editor where

import Prelude

import Ace (EditSession, Editor, ace, editNode)
import Ace.EditSession (getValue, setValue)
import Ace.Editor (getSession)
import CSS (display, flex, flexDirection, height, px, row, width)
import Control.Monad.Rec.Class (class MonadRec)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse_)
import Data.Tuple.Nested (type (/\), (/\))
import Effect.Aff.Class (class MonadAff)
import Examples.LevelDB.FileSystem (FilePath, FileSystem, openFileSystem, readFile, writeFile)
import Halogen (RefLabel(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS (style)
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Web.Event.Event (Event, stopPropagation)
import Web.HTML.HTMLElement (fromElement)
import Web.UIEvent.MouseEvent (toEvent)

type State =
  { editSession :: Maybe (FileSystem /\ Editor /\ EditSession)
  , filePath :: FilePath
  }

data Action =
    Initialize
  | Save Event
  | Exit Event

component :: forall q m. MonadAff m => MonadRec m => H.Component q FilePath Unit m
component = do
  H.mkComponent
    { initialState: \filePath -> { editSession: Nothing, filePath } 
    , render: renderComponent
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction
                                     , initialize = Just Initialize
                                     }
    }

renderComponent :: forall m. MonadAff m => State -> H.ComponentHTML Action () m
renderComponent _ =
  let editor =
        HH.div_
          [ HH.div [ HP.ref (RefLabel "editor")
                   , style do
                       width (px 500.0)
                       height (px 500.0)
                   ] []
          , HH.button [ HE.onClick (Save <<< toEvent)] [HH.text "Save" ] 
          , HH.button [ HE.onClick (Exit <<< toEvent)] [HH.text "Exit" ] 
          ]
  
   in
     HH.div
       [ style do
           display flex
           flexDirection row
       ]
       [ editor 
       ]


handleAction :: forall m .
                MonadAff m
             => MonadRec m
             => Action
             -> H.HalogenM State Action () Unit m Unit
handleAction = case _ of
  Initialize -> do
    fs <- H.liftEffect openFileSystem
    fp <- H.gets (\st -> st.filePath)
    fi <- H.liftAff $ readFile fs fp
    ref <- H.getRef (RefLabel "editor")
    flip traverse_ ref $ \el -> do
       flip traverse_ (fromElement el) $ \ht -> do
         ed <- H.liftEffect $ editNode ht ace
         sess <- H.liftEffect $ getSession ed
         flip traverse_ fi $ \txt -> do
            H.liftEffect $ setValue txt sess
         H.modify_ (\st -> st { editSession = (Just (fs /\ ed /\ sess)) })
  Save e -> do
     H.liftEffect $ stopPropagation e
     s <- H.gets (\st -> st.editSession)
     p <- H.gets (\st -> st.filePath)
     flip traverse_ s $ \(db /\ _ /\ sess) -> do
        f <- H.liftEffect $ getValue sess
        H.liftAff $ writeFile db p f
  Exit e -> do
     H.liftEffect $ stopPropagation e
     H.raise unit 



