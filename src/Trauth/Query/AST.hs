{-# LANGUAGE LambdaCase #-}
module Trauth.Query.AST where

import Debug.Todo
import Data.Char (toLower)
import Data.Either (rights)
import Control.Monad.State
import qualified Data.HashMap as Map
import Data.HashMap (Map)

data Context = Context {arguments :: [String],
                        consumed :: [String]}

type CommandReader = StateT Context (Either String)

data Command = Runnable Int ([String] -> IO ())
             | Branch (Map String Command)

instance Monoid Command where
  mempty = Branch Map.empty
  mappend (Branch cs1) (Branch cs2)
    = Branch $ Map.union cs1 cs2

  mappend (Runnable a1 f1) (Runnable a2 f2)
    = let a = max a1 a2
          f args = f1 (take a1 args) >> f2 (take a2 args)
      in Runnable a f

mkCom :: String -> Int -> ([String] -> IO ()) -> Command
mkCom invoker arity f = cc (tokenize invoker)
  where cc :: [String] -> Command
        cc = \case (x:xs) -> Branch $ Map.singleton x $ cc xs
                   []     -> Runnable arity f

exampleCommands
  = mconcat [mkCom "help"     0 $ \[] -> putStrLn "hello!",
             mkCom "get card" 1 $ \[x] -> putStrLn $ "fetching card: " ++ x,
             mkCom "set bard" 2 $ \[x, y] -> putStrLn $ concat [
                "setting bard '", x, "' to '", y, "'"
                ]
            ]

run :: String -> Command -> Either String (IO ())
run query command = apply <$> runStateT (eval command) ctx
  where apply (f, Context as _cs) = f as
        ctx = Context (tokenize query) []


eval :: Command -> CommandReader ([String] -> IO ())
eval (Runnable arity function) = do
  fLen <- length . arguments <$> get
  unless (arity == fLen) $ do
    consed <- strConsumed
    fail $ concat ["Wrong number of arguments for '", consed, "'",
                   "\nExpected ", show arity, ", got ", show fLen]
  return function
eval (Branch branch) = unlessEmpty $ do
  arg <- head . arguments <$> get
  modify $ \(Context (a:as) cs) -> Context as (a:cs)

  case Map.lookup arg branch of
    Just c -> eval c
    Nothing -> do
      cs <- strConsumed
      fail $ concat ["No such command '", cs, "'"]

    where unlessEmpty m = (arguments <$> get) >>= \case
            [] -> do cs <- strConsumed
                     fail $ concat ["Not enough arguments after '", cs, "'"]
            _  -> m

tokenize :: String -> [String]
tokenize = tokenize' [] []
  where tokenize' tokens buf (x:y:xs)
          | all (`elem` ws) [x, y]  = tokenize' tokens buf (y:xs)
        tokenize' tokens buf (x:xs)
          | x `elem` ws             = tokenize' (reverse buf:tokens) [] xs
        tokenize' tokens buf (x:xs) = tokenize' tokens (toLower x:buf) xs
        tokenize' tokens buf []     = reverse $ reverse buf:tokens

        ws = [' ', '\n', '\t']

strConsumed :: CommandReader String
strConsumed = unwords . reverse . consumed <$> get
