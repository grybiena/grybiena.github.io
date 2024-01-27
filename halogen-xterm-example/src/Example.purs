module Example where

import Prelude

import Control.Monad.Rec.Class (class MonadRec)
import Data.Lens ((.~), (^.))
import Data.Maybe (Maybe(..))
import Data.String (joinWith)
import Data.String as String
import Data.Traversable (traverse_)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff.AVar as AVar
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Example.Button (Query(..))
import Example.Button as Button
import Example.Editor as Editor
import Example.FileSystem (FilePath(..), deleteFile, listFiles, openFileSystem)
import Halogen as H
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Halogen.XShell (closeWindow, openWindow, queryWindow)
import Halogen.XShell as Shell
import Halogen.XShell.CommandLine (Command, ShellState(..), canceler, prog, cmd, commandLine, prompt, textInterpreter)
import Halogen.XShell.Free (getShell, interpreter, modifyShell, terminal)
import Halogen.XTerm.Free (loadAddons, options, rows, write, writeLn)
import Halogen.XTerm.Free.Options (getCursorBlink, getFontFamily, setCursorBlink)
import Type.Proxy (Proxy(..))


main :: Effect Unit
main = do
  HA.runHalogenAff do
     body <- HA.awaitBody
     stdout <- AVar.empty
     let shell :: ShellState Slots Unit Aff
         shell = ShellState { prompt: "> ", command: "", foreground: Nothing, commandEnv: commands }
     io <- runUI Shell.component shell body
     void $ io.query $ do
       terminal do
         loadAddons true
         write "> "
       interpreter (textInterpreter $ commandLine (prog commands)) 
     AVar.put io.query stdout

type Slots =
  ( button :: H.Slot Button.Query Unit Unit 
  , editor :: H.Slot Maybe Unit Unit
  )

_button = Proxy :: Proxy "button"
_editor = Proxy :: Proxy "editor"

commands :: forall o m. MonadAff m => MonadRec m => Array (Command Slots o m)
commands =
  [ { name: "rows"
    , description: [ "prints the number of rows in the terminal" ]
    , cmd: basic (terminal $ rows >>= writeLn <<< append "\r\n" <<< show)
    }
  , { name: "cols"
    , description: [ "prints the number of columns in the terminal" ]
    , cmd: basic (terminal $ rows >>= writeLn <<< append "\r\n" <<< show)
    }
  , { name: "blinking"
    , description: [ "prints a boolean indicating whether the cursor is blinking" ]
    , cmd: basic (terminal $ options getCursorBlink >>= writeLn <<< append "\r\n" <<< show)
    }
  , { name: "blinkon"
    , description: [ "turns on cursor blinking" ]
    , cmd: basic (terminal $ options $ setCursorBlink true)
    }
  , { name: "blinkoff"
    , description: [ "turns off cursor blinking" ]
    , cmd: basic (terminal $ options $ setCursorBlink false)
    } 
  , { name: "fontFamily"
    , description: [ "prints the font family of the terminal" ]
    , cmd: basic (terminal $ options getFontFamily >>= writeLn <<< append "\r\n" <<< show)
    }
  , { name: "button"
    , description: [ "creates a button as a subprocess."
                   , "the subprocess can be cancelled with ^C"
                   , "accepts button text as argument."
                   , "clicking the button prints \"click!\" to the terminal."
                   ]
    , cmd: \args -> do 
        modifyShell (cmd .~ "")
        openWindow _button unit Button.component (joinWith " " args) (const $ terminal $ writeLn "click!")
        let proc =  { stdin: \txt -> void $ queryWindow _button unit (SetText txt unit) 
                    , kill: closeWindow _button unit
                    }
        modifyShell (\(ShellState s) -> ShellState s { foreground = Just proc })
        terminal do
           options $ setCursorBlink false
           write "\r\n"
        interpreter canceler
    } 
  , { name: "edit"
    , description: [ "opens a file in the editor as a subprocess."
                   , "the subprocess can be cancelled with ^C"
                   , "accepts the file path as argument."
                   ]
    , cmd: \args -> do 
        case args of
          [fp] | String.length fp > 0 -> do
            modifyShell (cmd .~ "")
            let exit = do
                  closeWindow _editor unit
                  terminal $ write "> "
                  interpreter (textInterpreter $ commandLine (prog commands)) 
            openWindow _editor unit Editor.component (FilePath fp) (const exit) 
            let proc = { stdin: const $ pure unit
                       , kill: closeWindow _editor unit
                       }
            modifyShell (\(ShellState s) -> ShellState s { foreground = Just proc })
            terminal do
               options $ setCursorBlink false
               write "\r\n"
            interpreter canceler
          _ -> basic (terminal $ writeLn "\r\nthis command expects a single file name as its argument") args
    } 
  , { name: "ls"
    , description: [ "lists files in the file system" ]
    , cmd: basic $ do
       fs <- liftEffect $ openFileSystem
       files <- liftAff $ listFiles fs
       terminal $ writeLn ("\r\n" <> (joinWith " " ((\(FilePath fp) -> fp) <$> files)))
    }
  , { name: "rm"
    , description: [ "remove files from the file system" ]
    , cmd: \args -> do
       fs <- liftEffect $ openFileSystem
       traverse_ (\f -> liftAff $ deleteFile fs (FilePath f)) args
       done
    }
  ]
  where
    basic c = const (c *> done)
    done = do
       shell' <- getShell
       terminal $ write ("\r\n" <> shell' ^. prompt)
       modifyShell (cmd .~ "")

