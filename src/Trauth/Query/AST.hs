{-# LANGUAGE GADTs, LambdaCase #-}
module Trauth.Query.AST where

import Debug.Todo
import Data.Char (toLower)
import Data.List (intercalate)
import Control.Monad.State

data Object = Card

data Context = Context {arguments :: [String],
                        consumed :: [String]}

type CommandReader = StateT Context (Either String)

newtype Command = Command {getCD :: [(String, CommandDispatcher)]}

data CommandDispatcher = CDCommand   Command
                       | CDRunnable ([String] -> IO ())

run :: String -> Command -> Either String (IO ())
run query command = do
  (f, Context as _cs) <- runStateT (eval command) (Context (tokenize query) [])
  return $ f as

eval :: Command -> CommandReader ([String] -> IO ())
eval c = unlessEmpty $ do
  arg <- head . arguments <$> get
  modify $ \(Context (a:as) cs) -> Context as (a:cs)

  case getCommandDispatcher c arg of
    Just (CDCommand c') -> eval c'
    Just (CDRunnable f) -> return f
    Nothing -> do
      cs <- getConsumed
      fail $ concat ["No such command '", cs, "'"]

    where getConsumed = unwords . reverse . consumed <$> get
          unlessEmpty m = (arguments <$> get) >>= \case
            [] -> do cs <- getConsumed
                     fail $ concat ["Not enough arguments after '", cs, "'"]
            _  -> m




tokenize :: String -> [String]
tokenize = reverse . tokenize' [] []
  where tokenize' tokens buf (x:y:xs)
          | all (`elem` ws) [x, y]  = tokenize' tokens buf (y:xs)
        tokenize' tokens buf (x:xs)
          | x `elem` ws             = tokenize' (reverse buf:tokens) [] xs
        tokenize' tokens buf (x:xs) = tokenize' tokens (toLower x:buf) xs
        tokenize' tokens buf []     = reverse $ reverse buf:tokens

        ws = [' ', '\n', '\t']

dispatchers :: Command -> [String]
dispatchers = map fst . getCD

getCommandDispatcher :: Command -> String -> Maybe CommandDispatcher
getCommandDispatcher c s = lookup s $ getCD c



getObjectType :: String -> Either String Object
getObjectType "card" = Right Card
getObjectType t = Left $ "Unknown Trello Object '"++ t ++"'"
