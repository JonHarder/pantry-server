{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Model where

import Data.Aeson.Types
import Database.PostgreSQL.Simple
import GHC.Generics

data Recipe = Recipe
  { name :: String
  , ingredients :: [String]
  , instructions :: [String]
  } deriving Generic

instance ToJSON Recipe
instance FromJSON Recipe


getRecipeById :: Connection -> Int -> IO (Maybe Recipe)
getRecipeById conn recipeId = do
  results <- query conn "select id, name from recipe where id = ?" $ Only recipeId :: IO [(Int, String)]
  let recipes = map (\(rId, rName) -> Recipe rName [] []) results
  if (not . null) recipes then
    return $ Just $ head recipes
  else
    return Nothing
