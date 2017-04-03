{-# LANGUAGE LambdaCase, RankNTypes #-}
module Trauth.Utils.TokenCache where

import Control.Exception
import Text.Read
import System.Directory
import qualified System.FilePath.Posix as Path

type Key = FilePath

fp :: Key -> IO FilePath
fp k = let userPath = "/.config/trauth/" ++ k
       in (++ userPath) <$> getHomeDirectory

showExp :: IOException -> IO (Either String a)
showExp e = return $ Left (displayException e)

save :: (Show a, Read a) => a -> Key -> IO (Either String ())
save datas key = unsafeSave datas key `catch` showExp
  where unsafeSave datas key = do
          path <- fp key
          createDirectoryIfMissing True $ Path.takeDirectory path
          writeFile path $ show datas
          return $ Right ()

load :: (Read a, Show a) => Key -> IO (Either String a)
load key = unsafeLoad key `catch` showExp
  where unsafeLoad key = do
          path <- fp key
          doesFileExist path >>= \case
            True  -> readEither <$> readFile path
            False -> return . Left $ mconcat ["File '", path, "' not found!"]
