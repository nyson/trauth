{-# LANGUAGE LambdaCase #-}
module Trauth.REPL where

import System.Console.Readline
import Control.Monad (when)
import Data.IORef
import Trauth.TrelloM
import Trauth.TrelloRequests
import qualified Data.ByteString.Lazy.Char8 as LBS

enterToken = readline "Token: " >>= \case
  Just str -> return str
  Nothing  -> do
    putStrLn "You have to enter something!"
    enterToken


strip :: String -> String
strip "" = ""
strip (' ':str) = strip str
strip str = strip' str 0
  where
    strip' []        _      = []
    strip' (' ':str) spaces = strip' str $ spaces + 1
    strip' (c  :str) spaces = c:replicate spaces ' ' ++ strip' str 0


program :: Trello ()
program = do
  key <- trelloKey
  liftIO $ putStrLn . mconcat $ [
    "Welcome! To start, go to ", readTokenURL key,
    " and get your application token."]
  repl


readToken action = liftIO (readline "Enter token: ") >>= \case
    Just token -> setToken token >>= \case
      Left err  -> liftIO . putStrLn $ mconcat [
        "Bad token: '", token, "'"]
      Right ()  -> do
        liftIO $ putStrLn "Token set, continuing..."
        action
    Nothing    -> liftIO $ putStrLn "I didn't understand that!"

executeCommand :: String -> Trello Bool
executeCommand cmd = do
  continue <- liftIO $ newIORef True

  case cmd of
    url@('/':_) -> createURLAndMakeRequest url >>= \case
      Left err       -> tPutLn err
      Right response -> tPutLn $ LBS.unpack response
    (':':c) -> control continue c
    c -> unrecognized c
  liftIO $ readIORef continue

  where tPutLn = liftIO . putStrLn

control :: IORef Bool -> String -> Trello ()
control cont = \case
  c | c `elem` ["help", "h"] -> help
  c | c `elem` ["quit", "q"] -> liftIO $ do
        putStrLn "Bye!"
        cont `writeIORef` False
  c -> unrecognized c

unrecognized c = do
  liftIO . putStrLn $ mconcat [
      "Unrecognized command '", c, "'!"]
  help

help = liftIO . putStrLn $ mconcat [
  "Commands:",
  "\n\tAny url starting with / - Access trello api at this point with key and token set",
  "\n\t:token <token> - Set access token",
  "\n\t:quit - Quit TrAuth",
  "\n\t:help - This text you're currently reading",
  "\n\nMost commands can also be entered by the first letter of the command"
  ]

repl :: Trello ()
repl = trelloToken >>= \case
  Nothing -> readToken repl
  Just t -> do
    line <- liftIO $ readline "Trello> "
    continue <- case strip <$> line of
      Just str -> do
        liftIO $ addHistory str
        executeCommand str
      _ -> do
        liftIO $ putStrLn "I didn't recognize that command!"
        help
        return True

    when continue repl
