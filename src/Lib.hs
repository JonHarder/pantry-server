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


-- type API = "position" :> Capture "x" Int :> Capture "y" Int :> Get '[JSON] Position
--       :<|> "hello" :> QueryParam "name" String :> Get '[JSON] HelloMessage
--       :<|> "marketing" :> ReqBody '[JSON] ClientInfo :> Post '[JSON] Email
--       :<|> "err" :> Get '[JSON] ()


type API = "recipes" :> Get '[JSON] [Recipe]
      :<|> "recipes" :> Capture "recipeId" Int :> Get '[JSON] (Maybe Recipe)
      :<|> "recipes" :> ReqBody '[JSON] Recipe :> Post '[JSON] RecipePostResponse


data RecipePostResponse
  = Sucess { recipeId :: Int }
  | Failure { error :: String }
  deriving Generic

instance ToJSON RecipePostResponse


data Recipe = Recipe
  { name :: String
  , ingredients :: [String]
  , instructions :: [String]
  } deriving Generic

instance ToJSON Recipe
instance FromJSON Recipe


-- server :: Server API
-- server = position
--     :<|> hello
--     :<|> marketing
--     :<|> (throwError $ err404 { errBody = "error: error not found" })
        

server :: Server API
server = recipes
    :<|> recipe
    :<|> recipePost
  where recipes = do
          liftIO $ putStrLn "getting all the recipes"
          return [keyLime]
        recipe recipeId = return $
          if recipeId == 2 then
            Just keyLime
          else
            Nothing
        recipePost recipe = do
          liftIO $ putStrLn $ "saving recipe: " ++ name recipe
          return $ Sucess 1
        keyLime = Recipe "Key Lime Pie" ["limes"] ["smash the limes"]


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
