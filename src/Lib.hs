{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Lib (
  runApp
  ) where

import Prelude ()
import Prelude.Compat

import Control.Monad.Except
import Data.Aeson.Types
import Data.List
import Data.Maybe (fromMaybe)
import GHC.Generics
import Network.Wai.Handler.Warp
import Servant
import Text.Printf (printf)
import Text.Read (readMaybe)
import System.Environment
import System.IO


type API = "position" :> Capture "x" Int :> Capture "y" Int :> Get '[JSON] Position
      :<|> "hello" :> QueryParam "name" String :> Get '[JSON] HelloMessage
      :<|> "marketing" :> ReqBody '[JSON] ClientInfo :> Post '[JSON] Email
      :<|> "err" :> Get '[JSON] ()


data Position = Position
  { xCoord :: Int
  , yCoord :: Int
  } deriving Generic

instance ToJSON Position

newtype HelloMessage = HelloMessage { msg :: String }
  deriving Generic

instance ToJSON HelloMessage

data ClientInfo = ClientInfo
  { clientName :: String
  , clientEmail :: String
  , clientAge :: Int
  , clientInterestedIn :: [String]
  } deriving Generic

instance FromJSON ClientInfo
instance ToJSON ClientInfo

data Email = Email
  { from :: String
  , to :: String
  , subject :: String
  , body :: String
  } deriving Generic

instance ToJSON Email


emailForClient :: ClientInfo -> Email
emailForClient c = Email from' to' subject' body'
  where from' = "great@company.com"
        to' = clientEmail c
        subject' = "Hey " ++ clientName c ++ ", we miss you!"
        body' = "Hi " ++ clientName c ++ ",\n\n"
             ++ "Since you've recently turned " ++ show (clientAge c)
             ++ ", have you checked out our latest "
             ++ intercalate ", " (clientInterestedIn c)
             ++ " products? Give us a visit!"


server :: Server API
server = position
    :<|> hello
    :<|> marketing
    :<|> (throwError $ err404 { errBody = "error: error not found" })

  where position :: Int -> Int -> Handler Position
        position x y = return (Position x y)

        hello :: Maybe String -> Handler HelloMessage
        hello mname = return . HelloMessage $ case mname of
          Nothing -> "Hello, anonymous person"
          Just n -> "Hello, " ++ n

        marketing :: ClientInfo -> Handler Email
        marketing clientInfo = do
          liftIO $ printf "sendint an email to %s\n" (clientName clientInfo)
          return (emailForClient clientInfo)
        


api :: Proxy API
api = Proxy


app :: Application
app = serve api server


readEnv :: Read a => String -> a -> IO a
readEnv var d = do
  val <- lookupEnv var
  return $ fromMaybe d (val >>= readMaybe)


runApp :: IO ()
runApp = do
  port <- readEnv "APP_PORT" 8000
  printf "running server on http://localhost:%i\n" port
  hFlush stdout
  run port app
