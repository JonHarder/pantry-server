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
import Data.Maybe (fromMaybe)
import Database.PostgreSQL.Simple
import GHC.Generics
import Network.Wai.Handler.Warp
import Servant
import System.Environment
import System.IO

import Database.Connection
import Model

type API = "recipes" :> QueryParam "name" String :> Get '[JSON] [Recipe]
      :<|> "recipes" :> Capture "recipeId" Int :> Get '[JSON] (Maybe Recipe)
      :<|> "recipes" :> ReqBody '[JSON] Recipe :> Post '[JSON] RecipePostResponse
      :<|> "err"     :> Get '[JSON] ()


data RecipePostResponse
  = RecipePostSuccess { recipeId :: Int }
  | RecipePostFailure { error :: String }
  deriving Generic

instance ToJSON RecipePostResponse


getRecipes :: Maybe String -> Handler [Recipe]
getRecipes nameQuery = do
  conn <- liftIO getConnection
  case nameQuery of
    Just name ->
      liftIO $ findRecipesByName  conn name
    Nothing ->
      liftIO $ getAllRecipes conn

        
getRecipe :: Int -> Handler (Maybe Recipe)
getRecipe recipeId = do
  conn <- liftIO getConnection
  liftIO $ getRecipeById conn recipeId


postRecipe :: Recipe -> Handler RecipePostResponse
postRecipe r = undefined


server :: Server API
server = getRecipes
    :<|> getRecipe
    :<|> postRecipe
    :<|> (throwError $ err500 { errBody = "I just wanted to see what would happen"})


api :: Proxy API
api = Proxy


app :: Application
app = serve api server


runApp :: IO ()
runApp = do
  port <- read . fromMaybe "80" <$> lookupEnv "APP_PORT"
  putStrLn $ "running server on http://localhost:" ++ show port
  hFlush stdout
  run port app
