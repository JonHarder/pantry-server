module Database.Connection where


import Data.ByteString.Char8 (pack, unpack)
import Database.PostgreSQL.Simple
import System.Environment (lookupEnv)


getConnection :: IO Connection
getConnection = do
  dbHost <- lookupEnv "PANTRY_DB_SERVICE_SERVICE_HOST"
  case dbHost of
    Just host -> connect config
      where config = defaultConnectInfo
              { connectHost =     host
              , connectUser =     "pantry"
              , connectPassword = "secret"
              , connectDatabase = "pantry"
              }
    Nothing -> connect defaultConnectInfo
