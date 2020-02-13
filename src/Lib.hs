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


type API = "recipes" :> QueryParam "name" String :> Get '[JSON] [Recipe]
      :<|> "recipes" :> Capture "recipeId" Int :> Get '[JSON] (Maybe Recipe)
      :<|> "recipes" :> ReqBody '[JSON] Recipe :> Post '[JSON] RecipePostResponse
      :<|> "err"     :> Get '[JSON] ()


data RecipePostResponse
  = RecipePostSuccess { recipeId :: Int }
  | RecipePostFailure { error :: String }
  deriving Generic

instance ToJSON RecipePostResponse


data Recipe = Recipe
  { name :: String
  , ingredients :: [String]
  , instructions :: [String]
  } deriving Generic

instance ToJSON Recipe
instance FromJSON Recipe


keyLime :: Recipe
keyLime = Recipe "Key Lime Pie" ["limes"] ["smash the limes"]


bread :: Recipe
bread = Recipe "Sourdough Bread" ["flour", "water", "salt"] ["it's complicated...."]


recipes :: [Recipe]
recipes = [keyLime, bread]


getRecipes :: Maybe String -> Handler [Recipe]
getRecipes mName =
  case mName of
    Just name | name == "pie" -> return [keyLime]
              | name == "bread" -> return [bread]
              | otherwise -> return []
    Nothing ->
      return recipes

        
getRecipe :: Int -> Handler (Maybe Recipe)
getRecipe recipeId = return $
  if recipeId == 2 then
    Just keyLime
  else
    Nothing


postRecipe :: Recipe -> Handler RecipePostResponse
postRecipe r = do
  liftIO $ putStrLn $ "saving recipe: " ++ name r
  return $
    if name r == "foo" then
      RecipePostFailure "that name doesn't make any sense"
    else
      RecipePostSuccess 8


server :: Server API
server = getRecipes
    :<|> getRecipe
    :<|> postRecipe
    :<|> (throwError $ err500 { errBody = "I just wanted to see what would happen"})

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
