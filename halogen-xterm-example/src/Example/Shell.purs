module Example.Shell where

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
import Effect.Aff.Class (class MonadAff)
import Halogen.Shell.CommandLine (class CommandLine, cmd, commandLine, prompt, textInterpreter)
import Halogen.Shell.Free (ProcessHandle, ShellM, getShell, interpreter, kill, modifyShell, tell, terminal)
import Halogen.Terminal as Terminal
import Halogen.Terminal.Free (options, write, writeLn)
import Halogen.Terminal.Free.Options (setCursorBlink)



newtype State o m =
  State {
    prompt :: String
  , command :: String
  , commandEnv :: Array (Command o m)
  , foreground :: Maybe ProcessHandle
  }

instance CommandLine (State o m) where
  cmd = lens' (\(State s) -> s.command /\ (\c -> State (s { command = c }))) 
  prompt = lens' (\(State s) -> s.prompt /\ (\c -> State (s { prompt = c }))) 


canceler :: forall o m . MonadAff m => MonadRec m => Terminal.Output -> ShellM (State o m) o m Unit
canceler = textInterpreter $ (case _ of 
                                -- Ctrl+C
                                "\x0003" -> do
                                   terminal $ write "^C"
                                   cancel
                                s -> commandLine attach s)

cancel :: forall o m . MonadAff m => MonadRec m => ShellM (State o m) o m Unit
cancel = do 
  State sh <- getShell
  flip traverse_ sh.foreground $ \h -> do
     kill h
  modifyShell (\(State st) -> State (st { foreground = Nothing }))
  modifyShell (cmd .~ "")
  shell' <- getShell
  terminal do
    options $ setCursorBlink true
    write ("\r\n" <> (shell' ^. prompt))
  interpreter (textInterpreter $ commandLine (prog sh.commandEnv))


attach :: forall o m . MonadAff m => ShellM (State o m) o m Unit
attach = do
  sh@(State { foreground }) <- getShell
  flip traverse_ foreground $ \h -> do
    tell h (sh ^. cmd)
  terminal $ write "\r\n"
  modifyShell (cmd .~ "")

type Command o m =
  { name :: String
  , description :: Array String
  , cmd :: Array String -> ShellM (State o m) o m Unit
  }

commandMap :: forall o m. MonadAff m => MonadRec m => Array (Command o m) -> Map String (Array String -> ShellM (State  o m) o m Unit)
commandMap cmds = Map.insert "help" helpCmd (Map.fromFoldable ((\cmd -> cmd.name /\ cmd.cmd) <$> cmds))
  where
    helpCmd _ = do
       terminal $ writeLn $ helpText cmds
       shell' <- getShell
       terminal $ write (shell' ^. prompt)
       modifyShell (cmd .~ "")

    helpText a = helpTxt <> joinWith "" (cmdHelp <$> a) 
      where
        helpTxt = "\r\n  " <> "help" <> "\r\n    show this help text"
        cmdHelp cmd = "\r\n  " <> cmd.name <> "\r\n    " <> (joinWith "\r\n    " cmd.description)


prog :: forall o m . MonadAff m => MonadRec m => Array (Command o m) -> ShellM (State o m) o m Unit
prog commands = do
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


