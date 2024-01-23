module Example where

import Prelude

import Control.Monad.Rec.Class (class MonadRec)
import Data.Array (filter, head, tail)
import Data.Lens (lens', (.~), (^.))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.String (Pattern(..), joinWith, null, split)
import Data.Traversable (traverse_)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff.AVar as AVar
import Effect.Aff.Class (class MonadAff)
import Example.Button as Button
import Halogen.Aff as HA
import Halogen.Shell as Shell
import Halogen.Shell.CommandLine (class CommandLine, cmd, commandLine, prompt, textInterpreter)
import Halogen.Shell.Free (ProcessHandle, ShellM, exec, getShell, interpreter, kill, modifyShell, tell, terminal)
import Halogen.Terminal as Terminal
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
               interpreter (textInterpreter $ commandLine prog)
           , query: terminal <<< write
           , shell: State { prompt: "> ", command: "", foreground: Nothing }
           }
     io <- runUI Shell.component shell body
     AVar.put io.query stdout

newtype State =
  State {
    prompt :: String
  , command :: String
  , foreground :: Maybe ProcessHandle
  }

instance CommandLine State where
  cmd = lens' (\(State s) -> s.command /\ (\c -> State (s { command = c }))) 
  prompt = lens' (\(State s) -> s.prompt /\ (\c -> State (s { prompt = c }))) 


canceler :: forall o m . MonadAff m => MonadRec m => Terminal.Output -> ShellM State o m Unit
canceler = textInterpreter $ (case _ of 
                                -- Ctrl+C
                                "\x0003" -> do
                                   State sh <- getShell
                                   flip traverse_ sh.foreground $ \h -> do
                                      kill h
                                   modifyShell (\(State st) -> State (st { foreground = Nothing }))
                                   modifyShell (cmd .~ "")
                                   shell' <- getShell
                                   terminal do
                                     options $ setCursorBlink true
                                     write "^C"
                                     write ("\r\n" <> (shell' ^. prompt))
                                   interpreter (textInterpreter $ commandLine prog)

                                s -> commandLine inprog s)

inprog :: forall o m . MonadAff m => ShellM State o m Unit
inprog = do
  sh@(State { foreground }) <- getShell
  flip traverse_ foreground $ \h -> do
    tell h (sh ^. cmd)
  terminal $ write "\r\n"
  modifyShell (cmd .~ "")

prog :: forall o m . MonadAff m => MonadRec m => ShellM State o m Unit
prog = do
  let cmds = commandMap commands
  sh <- getShell
  let cmdArgs = filter (not <<< null) $ split (Pattern " ") (sh ^. cmd)
  case head cmdArgs of    
    Nothing -> do 
      terminal do
        writeLn $ ""
      shell' <- getShell
      terminal $ write (shell' ^. prompt)
      modifyShell (cmd .~ "")
    Just c ->
      case Map.lookup c cmds of
        Just run -> run $ maybe [] identity (tail cmdArgs)
        Nothing -> do
          terminal do
            writeLn $ "\r\nunrecognised command \"" <> c <> "\" - type \"help\" to see the available commands"
          shell' <- getShell
          terminal $ write (shell' ^. prompt)
          modifyShell (cmd .~ "")

type Command o m =
  { name :: String
  , description :: Array String
  , cmd :: Array String -> ShellM State o m Unit
  }

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
  ]
  where
    basic c = const (c *> done)
    done = do
       shell' <- getShell
       terminal $ write ("\r\n" <> shell' ^. prompt)
       modifyShell (cmd .~ "")


helpText :: forall o m. Array (Command o m) -> String 
helpText a = helpCmd <> joinWith "" (cmdHelp <$> a) 
  where
    helpCmd = "\r\n  " <> "help" <> "\r\n    show this help text"
    cmdHelp cmd = "\r\n  " <> cmd.name <> "\r\n    " <> (joinWith "\r\n    " cmd.description)

commandMap :: forall o m. MonadAff m => MonadRec m => Array (Command o m) -> Map String (Array String -> ShellM State o m Unit)
commandMap cmds = Map.insert "help" helpCmd (Map.fromFoldable ((\cmd -> cmd.name /\ cmd.cmd) <$> cmds))
  where
    helpCmd _ = do
       terminal $ writeLn $ helpText cmds
       shell' <- getShell
       terminal $ write (shell' ^. prompt)
       modifyShell (cmd .~ "")






