module Examples.Halogen.XTerm.Component where


import Prelude

import Color (rgb)
import Control.Monad.Rec.Class (class MonadRec)
import Data.Lens ((.~), (^.))
import Data.Maybe (Maybe(..))
import Data.String (joinWith)
import Data.String as String
import Data.Traversable (traverse_)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Examples.Halogen.Widgets.Button (Query(..))
import Examples.Halogen.Widgets.Button as Button
import Examples.Halogen.Widgets.Editor as Editor
import Examples.LevelDB.FileSystem (FilePath(..), deleteFile, listFiles, openFileSystem)
import Graphics.Canvas.Free (CanvasT, fillRect, fillText, rotate, setFillColor, translate)
import Halogen as H
import Halogen.Canvas as Canvas
import Halogen.Canvas.Animate as Animate

import Halogen.HTML as HH
import Halogen.XShell (closeWindow, openWindow, queryWindow)
import Halogen.XShell as Shell
import Halogen.XShell.CommandLine (Command, ShellState(..), canceler, prog, cmd, commandLine, prompt, textInterpreter)
import Halogen.XShell.Free (ShellM, getShell, interpreter, modifyShell, terminal)
import Halogen.XTerm.Free (loadAddons, options, rows, write, writeLn)
import Halogen.XTerm.Free.Options (getCursorBlink, getFontFamily, setCursorBlink)
import Type.Proxy (Proxy(..))
import Web.HTML.Window.AnimationFrame (DOMHighResTimestamp(..))


type Slots m = ( shell :: forall o. H.Slot (ShellM (Windows m) (ShellState (Windows m) o m) o m) o Unit ) 

_shell = Proxy :: Proxy "shell"

component :: forall q i o m. MonadAff m => MonadRec m => H.Component q i o m 
component =
  H.mkComponent
    { initialState: const unit 
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction
                                     , initialize = Just Initialize
                                     }
    }

data Action = Initialize

handleAction :: forall s o m. MonadAff m => MonadRec m => Action -> H.HalogenM s Action (Slots m) o m Unit 
handleAction =
  case _ of
    Initialize ->
      void $ H.query _shell unit $ do
        terminal do
          loadAddons true
          write "> "
        interpreter (textInterpreter $ commandLine (prog commands)) 

shell :: forall o m. MonadAff m => MonadRec m => ShellState (Windows m) o m 
shell = ShellState { prompt: "> ", command: "", foreground: Nothing, commandEnv: commands }

render :: forall s m. MonadAff m => MonadRec m => s -> H.ComponentHTML Action (Slots m) m 
render = const $ HH.slot_ _shell unit Shell.component shell 


type Windows m =
  ( button :: H.Slot Button.Query Unit Unit 
  , editor :: H.Slot Maybe Unit Unit
  , canvas :: H.Slot (CanvasT m) Unit Unit
  , animation :: H.Slot Maybe Unit Unit 
  , sketch :: H.Slot Maybe Unit Unit
  )

_button = Proxy :: Proxy "button"
_editor = Proxy :: Proxy "editor"
_canvas = Proxy :: Proxy "canvas"
_animation = Proxy :: Proxy "animation"
_sketch = Proxy :: Proxy "sketch"

commands :: forall o m. MonadAff m => MonadRec m => Array (Command (Windows m) o m)
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
  , { name: "canvas"
    , description: [ "canvas example" ]
    , cmd: \_ -> do
        modifyShell (cmd .~ "")
        openWindow _canvas unit Canvas.component { width: 200, height: 200 } (const $ pure unit)
        let proc =  { stdin: const $ pure unit 
                    , kill: closeWindow _canvas unit
                    }
            draw = do
              setFillColor $ rgb 255 255 200
              fillRect { x: 0.0, y: 0.0, width: 200.0, height: 200.0 }
              setFillColor $ rgb 0 0 0
              fillText "hello" { x: 50.0, y: 50.0 }
        void $ queryWindow _canvas unit draw
        modifyShell (\(ShellState s) -> ShellState s { foreground = Just proc })
        terminal do
           options $ setCursorBlink false
           write "\r\n"
        interpreter canceler
    }
  , { name: "animation"
    , description: [ "canvas animation example" ]
    , cmd: \_ -> do
        modifyShell (cmd .~ "")
        let animation (DOMHighResTimestamp t) = do 
              setFillColor $ rgb 255 255 200
              fillRect { x: 0.0, y: 0.0, width: 200.0, height: 200.0 }
              setFillColor $ rgb 0 0 0
              translate { translateX: 50.0, translateY: 50.0 }
              rotate (t / 1000.0)
              fillText "hello" { x: 0.0, y: 0.0 }
        openWindow _canvas unit Animate.component { dimensions: { width: 200, height: 200 }, animation } (const $ pure unit)
        let proc =  { stdin: const $ pure unit 
                    , kill: closeWindow _canvas unit
                    }
        modifyShell (\(ShellState s) -> ShellState s { foreground = Just proc })
        terminal do
           options $ setCursorBlink false
           write "\r\n"
        interpreter canceler
    }

  ]
  where
    basic c = const (c *> done)
    done = do
       shell' <- getShell
       terminal $ write ("\r\n" <> shell' ^. prompt)
       modifyShell (cmd .~ "")


