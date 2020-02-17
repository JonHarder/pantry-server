module Database.Connection where


import Data.ByteString.Char8 (pack, unpack)
import Database.PostgreSQL.Simple
import System.Environment (lookupEnv)


getConnection :: IO Connection
getConnection = do
  dbUrl <- lookupEnv "DB_URL"
  case dbUrl of
    Just url -> connectPostgreSQL $ pack url
    Nothing -> connect defaultConnectInfo
