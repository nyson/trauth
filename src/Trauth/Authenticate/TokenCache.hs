{-# LANGUAGE LambdaCase, RankNTypes #-}
module Trauth.Authenticate.TokenCache where

import Control.Exception
import Text.Read
import System.Directory
import Control.Monad (when)
import qualified System.FilePath.Posix as Path
import Web.Authenticate.OAuth (Credential)
type Key = FilePath

data Cache = Cache {cacheToken :: Credential}
  deriving(Read, Show)

class (Read a, Show a) => Serializable a

instance Serializable Cache

data CacheError a = Fixable String
                  | Unfixable String
                  | Ok a

data FileStatus = DoesNotExist
                | LackingPermissions
                | FileOk

fp :: Key -> IO FilePath
fp k = let userPath = "/.config/trauth/" ++ k
       in (++ userPath) <$> getHomeDirectory

permitted :: FilePath -> IO Bool
permitted f = (\p -> writable p && readable p) <$> getPermissions f

check :: FilePath -> IO FileStatus
check f = doesFileExist f >>= \case
  False -> return DoesNotExist
  True  -> permitted f >>= \case
    True  -> return FileOk
    False -> return LackingPermissions

showExp :: IOException -> IO (CacheError a)
showExp = return . Unfixable . displayException

-- | Saves a serializable value
save :: Serializable a => Key -> a -> IO (CacheError ())
save k d = unsafeSave d k `catch` showExp
  where unsafeSave datas key = do
          path <- fp key
          createDirectoryIfMissing True $ Path.takeDirectory path
          writeFile path $ show datas
          return $ Ok ()

-- | Loads a serializable value
load :: Serializable a => Key -> IO (CacheError a)
load k = unsafeLoad k `catch` showExp
  where unsafeLoad key = do
          path <- fp key
          check path >>= \case
            DoesNotExist       -> return . Fixable $ mconcat ["File '", path, "' not found!"]
            LackingPermissions -> return . Unfixable $ "You need Read/Write permissions on " ++ path
            FileOk             -> ceWrap . readEither <$> readFile path

-- | Removes a serializable value; will throw on error
remove :: Key -> IO ()
remove k = do
  path <- fp k
  exists <- doesFileExist path
  when exists $ removePathForcibly path

ceWrap :: Either String a -> CacheError a
ceWrap (Left str) = Unfixable str
ceWrap (Right a)  = Ok a

fixable :: CacheError a -> Bool
fixable = \case Unfixable _ -> False
                _           -> True


