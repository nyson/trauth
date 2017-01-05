{-# LANGUAGE LambdaCase #-}
module Trauth.TrelloRequests where

import Network.HTTP.Simple

import Data.Maybe (fromMaybe)
import Data.List (intercalate)
import Trauth.TrelloM
import qualified Data.ByteString.Lazy.Char8 as LBS

{- for auth and secrets, implement stuff that works with https://developers.trello.com/authorize -}

readTokenURL key = mconcat[
  "https://trello.com/1/connect?key=",
  key, "&name=MyApp&response_type=token"]

responseData :: Show a => Response a -> String
responseData r = unlines $ map mconcat [
  ["Status: ", show $ getResponseStatusCode r],
  [header "Headers"],
  [concatMap (\(n, h) -> mconcat [show n, ": ", show h, "\n"]) (getResponseHeaders r)],
  [header "Body"],
  [show $ getResponseBody r]
  ]
  where header :: String -> String
        header s = concat ["======================== ", s, " ========================"]

createURLAndMakeRequest :: String -> Trello (Either String LBS.ByteString)
createURLAndMakeRequest requestString = do
  valid <- validToken
  if valid
  then do
    url <- buildUrl requestString
    liftIO $ do
      putStrLn $ concat ["Executing url ", url, " now..."]
      req <- parseRequest url
      response <- httpLBS req
      case getResponseStatusCode response of
        200  -> return $ Right (getResponseBody response)
        code -> return . Left $ mconcat [
          "Bad HTTP status code ", show code,
          "!\nBody: ", show $ getResponseBody response
          ]
  else return $ Left "Bad token!"

buildUrl :: String -> Trello String
buildUrl request = do
  [k, t] <- mapM (<$> get) [appKey, token]
  return $ concat [
    "https://api.trello.com/1/",
    request,
    urlArgs . fromMaybe [] $ sequence [
        ("key=" ++) <$> k,
        ("token=" ++) <$> t
        ]
    ]

    where urlArgs [] = ""
          urlArgs (h:xs) = concat ["?", h, intercalate "&" xs]

tokenURL :: Trello String
tokenURL = readTokenURL <$> trelloKey

