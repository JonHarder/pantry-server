{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Model where

import Control.Monad (forM)
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
  results <- query conn "SELECT id, name FROM recipe WHERE id = ?" $ Only recipeId :: IO [(Int, String)]
  recipes <- forM results $ \(rId, rName) -> do
    instructionResults <- query conn "SELECT step FROM instruction WHERE recipe_id = ? ORDER BY step_number ASC" $ Only rId :: IO [Only String]
    let instructions = map (\(Only instruction) -> instruction) instructionResults
    return $ Recipe { name = rName, instructions = instructions, ingredients = [] }
  if (not . null) recipes then
    return $ Just $ head recipes
  else
    return Nothing
