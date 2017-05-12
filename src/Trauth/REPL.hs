{-# LANGUAGE LambdaCase #-}
module Trauth.REPL where

import System.Console.Readline
import Control.Monad (when)
import Data.IORef

import Data.Maybe (isJust)
import qualified Data.ByteString.Lazy.Char8 as LBS

-- enterToken = readline "Token: " >>= \case
--   Just str -> return str
--   Nothing  -> do
--     putStrLn "You have to enter something!"
--     enterToken

-- strip :: String -> String
-- strip "" = ""
-- strip (' ':str) = strip str
-- strip str = strip' str 0
--   where
--     strip' []        _      = []
--     strip' (' ':str) spaces = strip' str $ spaces + 1
--     strip' (c  :str) spaces = c:replicate spaces ' ' ++ strip' str 0

-- program :: IO ()
-- program = do
--   key <- trelloKey
--   liftIO $ putStrLn . mconcat $ [
--     "Welcome! To start, go to ", readTokenURL key,
--     " and get your application token."]
--   repl

-- executeCommand :: String -> IO Bool
-- executeCommand cmd = do
--   continue <- liftIO $ newIORef True

--   case cmd of
--     url@('/':_) -> createURLAndMakeRequest url >>= \case
--       Left err       -> tPutLn err
--       Right response -> tPutLn $ LBS.unpack response
--     (':':c) -> control continue c
--     c -> unrecognized c
--   liftIO $ readIORef continue

--   where tPutLn = liftIO . putStrLn

-- control :: IORef Bool -> String -> IO ()
-- control cont = \case
--   c | c `elem` ["help", "h"] -> help
--   c | c `elem` ["quit", "q"] -> liftIO $ do
--         putStrLn "Bye!"
--         cont `writeIORef` False
--   c | c `elem` ["token", "t"] -> readToken
--   c -> unrecognized c

-- unrecognized c = do
--   liftIO . putStrLn $ mconcat [
--       "Unrecognized command '", c, "'!"]
--   help

-- help = liftIO . putStrLn $ mconcat [
--   "Commands:",
--   "\n\tAny url starting with / - Access trello api at this point with key and token set",
--   "\n\t:token <token> - Set access token",
--   "\n\t:quit - Quit TrAuth",
--   "\n\t:help - This text you're currently reading",
--   "\n\nMost commands can also be entered by the first letter of the command"
--   ]

-- repl :: Trello ()
-- repl = do
--   isJust <$> trelloToken >>= flip when
--     (liftIO $ putStrLn "No token found! You can assign one by writing :token")
--   line <- liftIO $ readline "Trello> "
--   continue <- case strip <$> line of
--     Just str -> do
--       liftIO $ addHistory str
--       executeCommand str
--     _ -> do
--       liftIO $ putStrLn "I didn't recognize that command!"
--       help
--       return True
--   when continue repl
