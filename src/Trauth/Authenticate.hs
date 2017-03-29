{-# LANGUAGE OverloadedStrings #-}
module Trauth.Authenticate where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Trauth.Authenticate.Secrets as Secrets
import Web.Authenticate.OAuth
import Network.HTTP.Conduit
import Data.Aeson
import Data.Text (Text)
import qualified Data.Text as T

data Card = Card {
  id :: Text,
  closed :: Bool,
  desc :: Text,
  board :: Text,
  labels :: [Text],
  name :: Text,
  url :: Text
  } deriving Show

instance FromJSON Card where
  parseJSON (Object o) = Card
    <$> o .: "id"
    <*> o .: "closed"
    <*> o .: "desc"
    <*> o .: "idBoard"
    <*> o .: "idLabels"
    <*> o .: "name"
    <*> o .: "shortUrl"

oauthSettings = newOAuth {
  oauthRequestUri=     "https://trello.com/1/OAuthGetRequestToken",
  oauthAuthorizeUri=   "https://trello.com/1/OAuthAuthorizeToken",
  oauthAccessTokenUri= "https://trello.com/1/OAuthGetAccessToken",
  oauthConsumerKey=    "11ed61ffbdc815615494cd41b93b5350",
  oauthConsumerSecret= Secrets.secret,
  oauthVersion=        OAuth10a
  }

withOAuth :: Manager -> Credential -> Request -> IO (Response LBS.ByteString)
withOAuth httpManager tokenizedCred req = do
  authedReq <- signOAuth oauthSettings tokenizedCred req
  httpLbs authedReq httpManager

withOAuth' :: FromJSON a => Manager -> Credential -> Request -> IO (Maybe a)
withOAuth' m c r = withOAuth m c r >>= return . decode . responseBody

flowExample :: IO ()
flowExample = do
  putStrLn $ "This is your settings: " ++ show oauthSettings
  putStrLn "Creating a manager..."
  man <- newManager tlsManagerSettings
  ok

  line

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
  ok

  line

  putStrLn "Let's try to access a card..."
  resp <- parseRequest "https://api.trello.com/1/cards/G0qATeAL/" >>= withOAuth man tok
  LBS.putStrLn $ responseBody resp
  ok

  line

  putStrLn "Let's try to get a card object"
  Just card <- parseRequest "https://api.trello.com/1/cards/G0qATeAL/" >>= withOAuth' man tok
  print (card :: Card)
  ok

  putStrLn "Bye!"

  where ok  = putStrLn "Ok!"
        ok' = putStrLn . ("Ok: " ++)
        line = putStrLn $ "\n" ++ replicate 80 '>' ++ "\n"
        enter str = putStr str >> BS.getLine
