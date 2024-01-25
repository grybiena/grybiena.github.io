module Example where

import Prelude

import Control.Monad.Rec.Class (class MonadRec)
import Data.Lens ((.~), (^.))
import Data.Maybe (Maybe(..))
import Data.String (joinWith)
import Data.Traversable (traverse_)
import Effect (Effect)
import Effect.Aff.AVar as AVar
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Example.Button as Button
import Example.Editor as Editor
import Example.FileSystem (FilePath(..), deleteFile, listFiles, openFileSystem)
import Example.Shell (State(..), Command, canceler, prog)
import Halogen.Aff as HA
import Halogen.Shell as Shell
import Halogen.Shell.CommandLine (cmd, commandLine, prompt, textInterpreter)
import Halogen.Shell.Free (exec, getShell, interpreter, modifyShell, terminal)
import Halogen.Terminal.Free (loadAddons, options, rows, write, writeLn)
import Halogen.Terminal.Free.Options (getCursorBlink, getFontFamily, setCursorBlink)
import Halogen.VDom.Driver (runUI)


main :: Effect Unit
main = do
  HA.runHalogenAff do
     body <- HA.awaitBody
     stdout <- AVar.empty
     let shell =
           { init: do
               terminal do
                 loadAddons true
                 write "> "
               interpreter (textInterpreter $ commandLine (prog commands))
           , query: terminal <<< write
           , shell: State { prompt: "> ", command: "", foreground: Nothing, commandEnv: commands }
           }
     io <- runUI Shell.component shell body
     AVar.put io.query stdout


commands :: forall o m. MonadAff m => MonadRec m => Array (Command o m)
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
        h <- exec Button.component args
        modifyShell (\(State s) -> State s { foreground = Just h })
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
          [_] -> do
            modifyShell (cmd .~ "")
            h <- exec Editor.component args
            modifyShell (\(State s) -> State s { foreground = Just h })
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


