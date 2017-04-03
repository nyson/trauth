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
import qualified Data.Text.IO as TextIO
import qualified Text.Regex as TRegex
import Data.Monoid ((<>))

import qualified Trauth.Utils.TokenCache as Cache

data Cache = Cache {cacheToken :: Text}
  deriving(Read, Show)

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

class ConsolePrettyPrint a where
  cpp :: a -> Int -> Text

instance ConsolePrettyPrint Card where
  cpp card w  = mconcat [
    header,
    description,

    body,
    "\\", T.replicate (w-2) "_", "/"
    ]
    where
      d = desc card
      description = mconcat ["| ", T.take (w-4) d, T.replicate (w-4-tlength' d) " ", " |\n"]
      header = mconcat ["/", T.replicate (w-2) "̅ ", "\\\n"]
      body = mconcat $ map field [
        mconcat ["Closed? ", if closed card then "Yes" else "No"]
        ]
      field t | T.length t > (w-4) = field $ T.take (w-7) t <> "..."
              | otherwise = mconcat ["| ", t, T.replicate (w-4-tlength' t) " "," |\n"]

      tlength' str = let rx = TRegex.mkRegex "\\e\\[[0-9]+m"
                         input = T.unpack str
                     in length $ TRegex.subRegex rx input  ""

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
withOAuth' m c r = withOAuth m c r >>= return . decode . responseBody

printExample :: IO ()
printExample = do
  raw <- LBS.readFile "examples/card.json"
  let Just card = (decode :: LBS.ByteString -> Maybe Card) raw
  TextIO.putStrLn $ cpp card 80

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
  TextIO.putStrLn $ cpp card 80
  ok

  putStrLn "Bye!"

  where ok  = putStrLn "Ok!"
        ok' = putStrLn . ("Ok: " ++)
        line = putStrLn $ "\n" ++ replicate 80 '>' ++ "\n"
        enter str = putStr str >> BS.getLine
