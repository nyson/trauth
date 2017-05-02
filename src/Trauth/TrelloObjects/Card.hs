{-# LANGUAGE OverloadedStrings #-}
module Trauth.TrelloObjects.Card where

import Data.Aeson
import Data.Aeson.Types (typeMismatch)
import Trauth.Utils.ConsolePrettyPrint

import Data.Text (Text)
import qualified Data.Text as T
import qualified Text.Regex as TRegex

import Data.Monoid ((<>))

data Card = Card {
  cId :: Text,
  cClosed :: Bool,
  cDesc :: Text,
  cBoard :: Text,
  cLabels :: [Text],
  cName :: Text,
  cUrl :: Text
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

instance ConsolePrettyPrint Card where
  cpp card w  = mconcat [
    header,
    description,

    body,
    "\\", T.replicate (w-2) "_", "/"
    ]
    where
      d = cDesc card
      description = mconcat ["| ", T.take (w-4) d, T.replicate (w-4-tlength' d) " ", " |\n"]
      header = mconcat ["/", T.replicate (w-2) "̅ ", "\\\n"]
      body = mconcat $ map field [
        mconcat ["Closed? ", if cClosed card then "Yes" else "No"]
        ]
      field t | T.length t > (w-4) = field $ T.take (w-7) t <> "..."
              | otherwise = mconcat ["| ", t, T.replicate (w-4-tlength' t) " "," |\n"]

      tlength' str = let rx = TRegex.mkRegex "\\e\\[[0-9]+m"
                         input = T.unpack str
                     in length $ TRegex.subRegex rx input  ""
