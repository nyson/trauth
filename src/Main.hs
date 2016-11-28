{-# LANGUAGE LambdaCase #-}

module Main where

import System.Environment

import Trauth.TrelloM
import Trauth.REPL
import Trauth.TrelloRequests

main :: IO ()
main = getArgs >>= \case
  [key] -> runTrello Read key program
  _     -> putStrLn "usage: trauth <key>"
