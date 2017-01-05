{-# LANGUAGE LambdaCase #-}

module Main where

import System.Environment

import Trauth.TrelloM
import Trauth.REPL
import Trauth.TrelloRequests
import Data.Default (def)

main :: IO ()
main = do
  args <- getArgs
  case parseArgs def args of
    Right state -> runTrello state program
    Left err -> putStrLn $ "Error: " ++ err

parseArgs :: TrelloState -> [String] -> Either String TrelloState
parseArgs c [] = Right c
parseArgs c xs = case parseArgList xs of
  Right (c', next) -> parseArgs c' next
  Left err         -> Left err

  where parseArgList = \case
          (k:v:next) | k `elem` ["--key", "-k"] ->
                       Right (c {appKey = Just v}, next)

                     | k `elem` ["--token", "-t"] ->
                       Right (c {token = Just v}, next)

          (o:next) | o == "-r" -> Right (c {mode = Read}, next)
                   | o == "-rw" -> Right (c {mode = ReadWrite}, next)

          c -> Left $ concat ["Unrecognized command: '", concat c, "'!"]

