{-# LANGUAGE OverloadedStrings, LambdaCase #-}
module Trauth.Authenticate where

import Control.Monad.State
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Trauth.Authenticate.Secrets as Secrets
import Web.Authenticate.OAuth
import Network.HTTP.Conduit
import Data.Aeson
import Data.Aeson.Types (typeMismatch)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TextIO
import qualified Text.Regex as TRegex
import Data.Monoid ((<>))

import Trauth.Utils.URL
import Trauth.Authenticate.TokenCache

getToken :: Credential -> String
getToken = BS8.unpack . snd . head . unCredential

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
  parseJSON e = typeMismatch "Card" e

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

type TrelloRequest = StateT (Manager, Credential) IO

runR :: Manager -> OAuth -> TrelloRequest () -> IO ()
runR man settings req = do
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

loadToken :: TrelloRequest (Maybe Credential)
loadToken = do
  liftIO (load "token") >>= \case
    Ok (Cache credential) -> Just <$> do
      undefined
    _                     -> return Nothing
  undefined

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
