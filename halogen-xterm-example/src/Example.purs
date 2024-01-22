module Example where

import Prelude

import Control.Monad.Rec.Class (class MonadRec)
import Data.Array (drop, filter, head)
import Data.Lens (lens', (.~), (^.))
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), null, split, trim)
import Data.Traversable (traverse_)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff.AVar as AVar
import Effect.Aff.Class (class MonadAff)
import Example.Button as Button
import Halogen.Aff as HA
import Halogen.Shell as Shell
import Halogen.Shell.CommandLine (class CommandLine, cmd, commandLine, prompt, runRepl, textInterpreter)
import Halogen.Shell.Free (ProcessHandle, ShellM, exec, getShell, interpreter, kill, modifyShell, tell, terminal)
import Halogen.Terminal as Terminal
import Halogen.Terminal.Free (cols, loadAddons, options, rows, write)
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
  sh <- getShell
  case filter (not <<< null) $ split (Pattern " ") (sh ^. cmd) of    
    c | head c == Just "button" -> do
       modifyShell (cmd .~ "")
       h <- exec Button.component (drop 1 c)
       modifyShell (\(State s) -> State s { foreground = Just h })
       terminal do
          options $ setCursorBlink false
          write "\r\n"
       interpreter canceler
    _ -> runRepl repl

repl :: forall o m . String -> ShellM State o m String
repl s | trim s == "rows" = show <$> terminal rows 
repl s | trim s == "cols" = show <$> terminal cols
repl s | trim s == "blink" = show <$> terminal (options getCursorBlink)
repl s | trim s == "blinkon" = do
   terminal $ options $ setCursorBlink true
   pure ""
repl s | trim s == "blinkoff" = do
   terminal $ options $ setCursorBlink false
   pure ""
repl s | trim s == "fontFamily" = terminal $ options getFontFamily
repl s = pure $ 
      "unrecognised command \"" <> s <> "\"\r\n"
   <> "available commands [" <> "button" <> "," <> "rows" <> "," <> "cols" <> "," <> "blink" <> "," <> "blinkon" <> "," <> "blinkoff" <> "," <> "fontFamily"

