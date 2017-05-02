module Trauth.Utils.URL (
  URL,
  parse, build,
  addParam, removeParam

  ) where

import Data.List.Split (splitOn)
import Data.Char as Char
import Data.List (intercalate)
import Control.Monad (mapM)

data URL = URL {
  protocol    :: String,
  domain      :: String,
  credentials :: Maybe (Either (String, String) String),
  path        :: String,
  getParams   :: [(String, String)]
  } deriving Show

build :: URL -> String
build url = concat [protocol url, "://",
                    buildCredentials url,
                    domain url,
                    path url,
                    buildParams url
                   ]

parse :: String -> Either String URL
parse raw = fst <$> (parseProtocol (url, raw) >>= parseUserAndPassword >>= parsePathAndArgs)
  where url = URL {protocol=    "-",
                   domain=      "-",
                   credentials= Nothing,
                   path=        "-",
                   getParams=   []
                   }

addParam :: URL -> String -> String -> URL
addParam u k v = u {getParams= params}
  where params = (k,v): filter ((/= k) . fst) (getParams u)

removeParam :: URL -> String -> URL
removeParam u k = u {getParams= params}
  where params = filter ((/= k) . fst) (getParams u)

buildCredentials :: URL -> String
buildCredentials URL{credentials= cred} = case cred of
  Nothing -> ""
  Just (Left (user, password)) -> concat [user, ":", password, "@"]
  Just (Right user) -> user ++ "@"

buildParams :: URL -> String
buildParams URL{getParams= ps} = case ps of
  [] -> ""
  xs -> '?': (intercalate "&" . map (\(k,v) -> concat [k, "=", v])) xs

parseProtocol :: (URL, String) -> Either String (URL, String)
parseProtocol (url, str)
  | "http://"  == lowerTake 7 str = Right (url {protocol= "http"}, drop 7 str)
  | "https://" == lowerTake 8 str = Right (url {protocol= "https"}, drop 8 str)
  | otherwise = Left "bad protocol!"
  where lowerTake i = map Char.toLower . take i

parsePathAndArgs :: (URL, String) -> Either String (URL, String)
parsePathAndArgs (url, str) = do
  case splitOn "?" str of
    [path]         -> parsePath (url, path)
    [path, params] -> do
      (url', _) <- parsePath (url, path)
      parseArgs (url', params)
    _              -> Left ("Bad path: " ++ str)

parsePath :: (URL, String) -> Either String (URL, String)
parsePath (url, str) = case break (== '/') str of
  (onlyDomain, "")        -> Right (url {domain= onlyDomain,
                                         path=   ""},         str)
  (firstDomain, thenPath) -> Right (url {domain= firstDomain,
                                         path=   thenPath},   str)

parseUserAndPassword :: (URL, String) -> Either String (URL, String)
parseUserAndPassword (url, str) = case splitOn "@" str of
  a:xs:[] -> case splitOn ":" a of
    user:[]          -> Right (url{credentials= Just(Right user)           }, xs)
    user:password:[] -> Right (url{credentials= Just(Left (user, password))}, xs)
    _                -> Left "bad user/password (this shouldn't happen)"
  [xs]    -> Right (url {credentials= Nothing}, str)
  _       -> Left "No user/password in domain"

parseArgs :: (URL, String) -> Either String (URL, String)
parseArgs (url, str) = do
  params <- parseParams str
  return (url {getParams= params}, "")
  where parseParams raw = mapM splitAndTuple (splitOn "&" raw)
        splitAndTuple str = case splitOn "=" str of
          [k, v] -> Right (k,v)
          _      -> Left $ "Bad param format: " ++ str
