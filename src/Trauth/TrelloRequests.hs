{-# LANGUAGE LambdaCase #-}
module Trauth.TrelloRequests where

import Network.HTTP.Simple

import Trauth.TrelloM
import qualified Data.ByteString.Lazy as LBS

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

createURLAndMakeRequest :: String -> Trello (Either String (Response LBS.ByteString))
createURLAndMakeRequest requestString = do
  key <- trelloKey
  trelloToken >>= \case
    Nothing -> do
      liftIO $ putStrLn "Bad token found!"
      Left <$> return "Bad token"
    Just t -> do
      let url = concat ["https://api.trello.com/1/",
                                requestString,
                                "?key=", key,
                                "?token=", t
                               ]
      liftIO $ do
        putStrLn $ concat ["Executing url ", url, " now..."]
        req <- parseRequest url
        response <- httpLBS req
        case getResponseStatusCode response of
          200  -> return $ Right response
          code -> return . Left $ mconcat ["Bad HTTP status code ", show code, "!"]

tokenURL :: Trello String
tokenURL = readTokenURL <$> trelloKey

