{-# LANGUAGE LambdaCase #-}
module Trauth.TrelloM (
  Trello,
  TKey,
  TToken,
  TrelloMode(..),
  TrelloState(..),
  runTrello,
  trelloKey,
  trelloToken,
  setToken,
  liftIO
  )where

import Control.Monad.Trans.State
import Control.Monad.IO.Class(liftIO)
import Network.HTTP.Simple(parseRequest, getResponseStatusCode, httpNoBody)


type TKey = String
type TToken = String
data TrelloMode = Read
                | ReadWrite

type Trello = StateT TrelloState IO

data TrelloState = TS {appKey :: TKey,
                       token  :: Maybe TToken,
                       mode   :: TrelloMode}

runTrello :: TrelloMode -> TKey -> Trello () -> IO ()
runTrello _ key action = evalStateT action $ TS key Nothing Read

trelloKey :: Trello TKey
trelloKey = appKey <$> get

trelloToken :: Trello (Maybe TToken)
trelloToken = token <$> get >>= \case
  Nothing -> return Nothing
  Just token -> setToken token >>= \case
    Left _  -> return Nothing
    Right _ -> Just <$> return token

setToken :: TToken -> Trello (Either String ())
setToken t = validateToken t >>= \case
  Left err -> return $ Left err
  Right _  -> do
    modify $ \st -> st {token= Just t}
    return $ Right ()

validateToken :: TToken -> Trello (Either String ())
validateToken token = appKey <$> get >>= \key -> liftIO $ do
  req <- parseRequest $ mconcat [
    "https://api.trello.com/1/tokens/", token,
    "?key=", key
    ]
  response <- httpNoBody req
  case getResponseStatusCode response of
    200  -> return $ Right ()
    code -> return . Left $ mconcat ["Bad HTTP status code ", show code, "!"]

