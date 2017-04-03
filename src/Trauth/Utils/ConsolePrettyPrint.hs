{-# LANGUAGE OverloadedStrings #-}
module Trauth.Utils.ConsolePrettyPrint where

import Data.Text (Text)
import qualified Data.Text as T

class ConsolePrettyPrint a where
  cpp :: a -> Int -> Text


wordWrap :: Text -> Int -> [Text]
wordWrap t w
  | T.null t  = []
  | otherwise = T.take w t: wordWrap (T.drop w t) w

wordWrapIndent :: Text -> Int -> [Text]
wordWrapIndent t width = case wordWrap t (width - 2) of
  (l:ls) -> l:map ("  " `T.append`) ls
  []     -> []


