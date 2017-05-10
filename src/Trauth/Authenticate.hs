{-# LANGUAGE OverloadedStrings, LambdaCase, TupleSections #-}
module Trauth.Authenticate (
  tempRunR, runR,
  Credential(..),
  TrelloRequest,
  findCredentials, initiateVerification, tryVerification,
  persistVerification, setCred,
  parseOAuthRequest, performOAuthRequest, performOAuthRequest'
                           )where

import Debug.Todo

import Control.Monad.State
import Control.Exception (catch)

import qualified Data.Text.IO as TextIO
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy.Char8 as LBS

import Data.Aeson
import Web.Authenticate.OAuth
import Network.HTTP.Conduit
import qualified Network.HTTP.Types.Status as Status

import qualified Trauth.Authenticate.Secrets as Secrets
import Trauth.Utils.URL
import Trauth.Authenticate.TokenCache
import Trauth.TrelloObjects.Card
import Trauth.Utils.ConsolePrettyPrint

oauthSettings :: OAuth
oauthSettings = newOAuth {
  oauthRequestUri=     "https://trello.com/1/OAuthGetRequestToken",
  oauthAuthorizeUri=   "https://trello.com/1/OAuthAuthorizeToken",
  oauthAccessTokenUri= "https://trello.com/1/OAuthGetAccessToken",
  oauthConsumerKey=    Secrets.apiKey,
  oauthConsumerSecret= Secrets.secret,
  oauthVersion=        OAuth10a
  }

type TrelloRequest = StateT (Manager, Credential) IO

setCred :: Credential -> TrelloRequest ()
setCred cred = modify $ \(man, _oldCred) -> (man, cred)

runR :: Manager -> Credential -> TrelloRequest a -> IO a
runR man cred req = evalStateT req (man, cred)

tempRunR :: Manager -> TrelloRequest a -> IO a
tempRunR man req = do
  cred <- getTemporaryCredentialWithScope "read" oauthSettings man
  runR man cred req

withOAuth :: Request -> TrelloRequest (Response LBS.ByteString)
withOAuth req = do
  (man, cred) <- get
  authedReq <- liftIO $ signOAuth oauthSettings cred req
  liftIO $ httpLbs authedReq man

withOAuth' :: FromJSON a => Request -> TrelloRequest (Maybe a)
withOAuth' r = (decode . responseBody) <$> withOAuth r

parseOAuthRequest :: String -> TrelloRequest Request
parseOAuthRequest = liftIO . parseRequest

performOAuthRequest :: String -> TrelloRequest (Response LBS.ByteString)
performOAuthRequest req = parseOAuthRequest req >>= withOAuth

performOAuthRequest' :: FromJSON a => String -> TrelloRequest (Maybe a)
performOAuthRequest' req = parseOAuthRequest req >>= withOAuth'


findCredentials :: Manager -> IO (Maybe Credential)
findCredentials man = load "token" >>= \case
  Ok (Cache credential) -> validateCredential man credential
  _ -> return Nothing

validateCredential :: Manager -> Credential -> IO (Maybe Credential)
validateCredential man cred = do
  response <- parseRequest ("https://api.trello.com/1/token/"
                            ++ getToken cred
                            ++ "?key=" ++ BS8.unpack Secrets.apiKey)
              >>= flip httpLbs man
  return $ if responseStatus response == Status.ok200
           then Just cred
           else Nothing
  where
    getToken = BS8.unpack . snd . head . unCredential


persistVerification :: TrelloRequest (Maybe String)
persistVerification = do
  (man, cred) <- get
  tok <- getAccessToken oauthSettings cred man

  tokResponse <- liftIO $ save "token" (Cache tok)
  case tokResponse of
    Unfixable str -> return . Just $ "Couldn't save token: " ++ str
    Fixable str   -> return . Just $ "Couldn't save token: " ++ str
    _             -> return Nothing

verificationURL :: Credential -> Either String URL
verificationURL c = parse $ authorizeUrl' (\_ _ -> [("name", "Trauth")]) oauthSettings c

initiateVerification :: TrelloRequest (Either String URL)
initiateVerification = do
  man <- fst <$> get
  cred <- liftIO $ getTemporaryCredentialWithScope "read" oauthSettings man
  modify $ \(m, _cred) -> (m, cred)
  return $ verificationURL cred

tryVerification :: String -> TrelloRequest (Maybe Credential)
tryVerification verification = do
  modify $ \(man, cred) -> (man, flip injectVerifier cred $ BS8.pack verification)
  (man, cred) <- get
  try $ getAccessToken oauthSettings cred man
  where
    try action = liftIO $ (Just <$> action) `catch` handler
    handler = const (return Nothing) :: OAuthException -> IO (Maybe a)
