{-# LANGUAGE OverloadedStrings, LambdaCase #-}
module Trauth.Authenticate where

import Debug.Todo

import Control.Monad.State
import Control.Exception (catch, Exception)

import qualified Data.Text.IO as TextIO
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Trauth.Authenticate.Secrets as Secrets

import Data.Aeson
import Web.Authenticate.OAuth
import Network.HTTP.Conduit
import qualified Network.HTTP.Types.Status as Status

import Trauth.Utils.URL
import Trauth.Authenticate.TokenCache
import Trauth.TrelloObjects.Card
import Trauth.Utils.ConsolePrettyPrint

getToken :: Credential -> String
getToken = BS8.unpack . snd . head . unCredential

newtype Verification = Verification {unV :: (URL, Credential)}

veriCred :: Verification -> Credential
veriCred = snd . unV

veriUrl :: Verification -> String
veriUrl = build . fst . unV

veri :: String -> Credential -> Verification
veri s c = case parse s of
  Right url -> Verification (url, c)
  Left  err -> error $ "Bad url parse: " ++ err

type TrelloRequest = StateT (Manager, Credential) IO

runR :: Manager -> OAuth -> TrelloRequest () -> IO ()
runR man settings req = do
  todo "use the settings"
  todo "finish the run function"
  let cred = undefined
  evalStateT req (man, cred)

oauthSettings :: OAuth
oauthSettings = newOAuth {
  oauthRequestUri=     "https://trello.com/1/OAuthGetRequestToken",
  oauthAuthorizeUri=   "https://trello.com/1/OAuthAuthorizeToken",
  oauthAccessTokenUri= "https://trello.com/1/OAuthGetAccessToken",
  oauthConsumerKey=    Secrets.apiKey,
  oauthConsumerSecret= Secrets.secret,
  oauthVersion=        OAuth10a
  }

withOAuth :: Manager -> Credential -> Request -> IO (Response LBS.ByteString)
withOAuth httpManager tokenizedCred req = do
  authedReq <- signOAuth oauthSettings tokenizedCred req
  httpLbs authedReq httpManager

withOAuth' :: FromJSON a => Manager -> Credential -> Request -> IO (Maybe a)
withOAuth' m c r = (decode . responseBody) <$> withOAuth m c r

printExample :: IO ()
printExample = do
  raw <- LBS.readFile "examples/card.json"
  let Just card = (decode :: LBS.ByteString -> Maybe Card) raw
  TextIO.putStrLn $ cpp card 80

findCredentials :: TrelloRequest (Maybe Credential)
findCredentials = liftIO (load "token") >>= \case
  Ok (Cache credential) -> validateCredential credential
  _ -> return Nothing

validateCredential :: Credential -> TrelloRequest (Maybe Credential)
validateCredential cred = do
  (man, _key) <- get
  response <- parseRequest ("https://api.trello.com/1/token/" ++ getToken cred ++ "?key=" ++ BS8.unpack Secrets.apiKey)
              >>= flip httpLbs man
  return $ if responseStatus response == Status.ok200
           then Just cred
           else Nothing

initiateVerification :: TrelloRequest Verification
initiateVerification = do
  man <- fst <$> get
  cred <- liftIO $ getTemporaryCredentialWithScope "read" oauthSettings man
  initV cred
  where
    initV :: Monad m => Credential -> m Verification
    initV cred = let aUrl = authorizeUrl' (\_ _ -> [("name", "Trauth")]) oauthSettings cred
                 in return $ veri aUrl cred

tryVerification :: String -> Credential -> TrelloRequest (Maybe Credential)
tryVerification verification cred
  = fst <$> get >>= try . getAccessToken oauthSettings cred'
  where
    try action = liftIO $ (Just <$> action) `catch` handler
    handler = const (return Nothing) :: OAuthException -> IO (Maybe a)
    cred' = flip injectVerifier cred $ BS8.pack verification

realFlow :: TrelloRequest (Either Verification Credential)
realFlow = findCredentials >>= \case
    Just cred -> return $ Right cred
    Nothing   -> Left <$> initiateVerification


flowExample :: IO (Manager, Credential)
flowExample = do
  putStrLn $ "This is your settings: " ++ show oauthSettings
  putStrLn "Creating a manager..."
  man <- newManager tlsManagerSettings
  ok

  line

  tok <- load "token" >>= \case
    Unfixable str -> do
      putStrLn str
      return emptyCredential

    Fixable _ -> do
      putStrLn "Getting credentials..."
      cred <- getTemporaryCredentialWithScope "read" oauthSettings man
      ok

      line

      ok' $ "Get your verification string at "
        ++ authorizeUrl' (\_ _ -> [("name", "Trauth")]) oauthSettings cred
      cred' <- flip injectVerifier cred <$> enter "Verification: "

      line

      putStrLn "Let's try to get an access token..."
      tok <- getAccessToken oauthSettings cred' man

      save "token" (Cache tok) >>= \case
        Unfixable str -> putStrLn $ "Couldn't save token: " ++ str
        Fixable str   -> putStrLn $ "Couldn't save token: " ++ str
        _             -> ok' "saved token..."

      return tok

    Ok (Cache token) -> do
      let reqStr = mconcat ["https://api.trello.com/1/token/", getToken token,
                            "?key=", BS8.unpack $ oauthConsumerKey oauthSettings]
      putStrLn ("Reading token from server... " ++ reqStr)
      req <- parseRequest reqStr
      resp <- httpLbs req man
      LBS.putStrLn $ responseBody resp
      return token

  putStrLn "Let's try to access a card..."
  resp <- parseRequest "https://api.trello.com/1/cards/G0qATeAL/" >>= withOAuth man tok
  LBS.putStrLn $ responseBody resp
  ok

  line

  putStrLn "Let's try to get a card object"
  mcard <- parseRequest "https://api.trello.com/1/cards/G0qATeAL/" >>= withOAuth' man tok
  case mcard of
    Just card -> do
      print (card :: Card)
      TextIO.putStrLn $ cpp card 80

    Nothing   -> putStrLn "Couldn't fetch card"
  ok

  putStrLn "Bye!"

  return (man, tok)
  where ok  = putStrLn "Ok!"
        ok' = putStrLn . ("Ok: " ++)
        line = putStrLn $ "\n" ++ replicate 80 '>' ++ "\n"
        enter str = putStr str >> BS.getLine
