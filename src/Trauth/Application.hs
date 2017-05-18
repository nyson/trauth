{-# LANGUAGE LambdaCase #-}
module Trauth.Application where

import Network.HTTP.Conduit
import Control.Monad.State (liftIO)
import qualified Data.Text.IO as TextIO

import Trauth.TrelloObjects.Card
import Trauth.Authenticate
import qualified Trauth.Utils as U

import Trauth.REPL

app :: IO ()
app = do
  man <- newManager tlsManagerSettings
  cred <- loadOrCreateCredentials man
  runR man cred $ do
    liftIO $ putStrLn "got cred!"
    Just card <- performOAuthRequest' "https://api.trello.com/1/cards/G0qATeAL/"
    liftIO $ TextIO.putStrLn $ U.cpp (card :: Card) 80

loadOrCreateCredentials :: Manager -> IO Credential
loadOrCreateCredentials man = findCredentials man >>= \case
  Just cred -> return cred
  Nothing -> tempRunR man createCred

createCred :: TrelloRequest Credential
createCred = do
  Right url <- initiateVerification
  input <- liftIO $ do
    putStrLn $ "Please go to the URL below and "
      ++ "then enter the verification string\n" ++ U.build url
    putStr "Verification string> "
    getLine

  tryVerification input >>= \case
    Just cred -> do
      persistVerification >>= \case
        Just e  -> liftIO . putStrLn $ "Could not save token: \n" ++ e
        Nothing -> return ()
      return cred

    Nothing -> do
      liftIO $ putStrLn "Remote server didn't accept verification string!"
      createCred

