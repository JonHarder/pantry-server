module Database.Connection
  ( DbConfig (..)
  , envConfig
  , connectWithConfig
  ) where


import Data.ByteString.Char8 (pack)
import Database.PostgreSQL.Simple
import System.Environment (lookupEnv)


data DbConfig = Config
  { dbUser :: String
  , dbPassword :: String
  , dbHost :: String
  , dbName :: String
  }


readEnv :: String -> String -> IO String
readEnv var backup = do
  mVal <- lookupEnv var
  case mVal of
    Just val ->
      return val
    Nothing ->
      return backup


envConfig :: IO DbConfig
envConfig = do
  dbUser <- readEnv "DB_USER" "pantry"
  dbPass <- readEnv "DB_PASS" ""
  dbHost <- readEnv "DB_HOST" "pantry"
  dbName <- readEnv "DB_NAME" "postgres"
  return $ Config dbUser dbPass dbHost dbName


connectWithConfig :: DbConfig -> IO Connection
connectWithConfig config =
  let url = concat ["postgresql://", dbUser config, ":", dbPassword config, "@", dbHost config, "/", dbName config]
  in connectPostgreSQL $ pack url
