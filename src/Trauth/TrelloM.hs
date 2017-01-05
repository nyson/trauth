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
  validToken,
  setToken,
  liftIO,
  get
  )where

import Control.Monad.Trans.State
import Control.Monad.IO.Class(liftIO)
import Network.HTTP.Simple(parseRequest, getResponseStatusCode, httpNoBody)
import Data.Default
import Data.Maybe (fromMaybe)
import Data.Either (isRight)
import qualified Data.ByteString.Lazy.Char8 as LBS

type TKey = String
type TToken = String
data TrelloMode = Read
                | ReadWrite
                  deriving Show

type Trello = StateT TrelloState IO

data TrelloState = TS {appKey :: Maybe TKey,
                       token  :: Maybe TToken,
                       mode   :: TrelloMode}
                   deriving Show

instance Default TrelloState where
  def = TS Nothing Nothing Read

runTrello :: TrelloState -> Trello () -> IO ()
runTrello state action = evalStateT action state

trelloKey :: Trello TKey
trelloKey = fromMaybe "" . appKey <$> get

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

validToken :: Trello Bool
validToken = do
  token <$> get >>= \case
    Nothing -> return False
    Just t -> isRight <$> validateToken t

validateToken :: TToken -> Trello (Either String ())
validateToken token = appKey <$> get >>= \key -> liftIO $ do
  req <- parseRequest $ mconcat [
    "https://api.trello.com/1/tokens/", token,
    "?key=", fromMaybe "" key
    ]
  response <- httpNoBody req
  case getResponseStatusCode response of
    200  -> return $ Right ()
    code -> return . Left $ mconcat ["Bad HTTP status code ", show code, "!"]
