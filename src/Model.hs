{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Model (Recipe, findRecipesByName, getAllRecipes, getRecipeById) where

import Control.Monad (forM)
import Data.Aeson.Types
import Data.Maybe (listToMaybe)
import Database.PostgreSQL.Simple
import GHC.Generics

data Recipe = Recipe
  { name :: String
  , ingredients :: [String]
  , instructions :: [String]
  } deriving Generic

instance ToJSON Recipe
instance FromJSON Recipe


getInstructions :: Connection -> Int -> IO [String]
getInstructions conn recipeId = do
  instructionResults <- query conn "SELECT step FROM instruction WHERE recipe_id = ? ORDER BY step_number ASC" $ Only recipeId :: IO [Only String]
  return $ map (\(Only instruction) -> instruction) instructionResults


getIngredients :: Connection -> Int -> IO [String]
getIngredients conn recipeId = do
  ingredientResults <- query conn
    "SELECT amount, name FROM ingredient i INNER JOIN ingredient_type it ON i.ingredient_type_id = it.id WHERE recipe_id = ?" $ Only recipeId
  return $ map (\(ingAmount, ingName) -> ingAmount ++ " of " ++ ingName) ingredientResults


assembleRecipes :: Connection -> [(Int, String)] -> IO [Recipe]
assembleRecipes conn recipeFragments =
  forM recipeFragments $ \(rId, rName) -> do
    instructions <- getInstructions conn rId
    ingredients <- getIngredients conn rId
    return $ Recipe { name = rName, instructions = instructions, ingredients = ingredients }


getRecipeById :: Connection -> Int -> IO (Maybe Recipe)
getRecipeById conn recipeId = do
  results <- query conn "SELECT id, name FROM recipe WHERE id = ?" $ Only recipeId :: IO [(Int, String)]
  recipes <- assembleRecipes conn results
  return $ listToMaybe recipes


getAllRecipes :: Connection -> IO [Recipe]
getAllRecipes conn = do
  results <- query_ conn "SELECT id, name FROM recipe" :: IO [(Int, String)]
  assembleRecipes conn results


findRecipesByName :: Connection -> String -> IO [Recipe]
findRecipesByName conn recipeName = do
  results <- query conn "select id, name from recipe where name like ?" $ Only ("%" ++ recipeName ++ "%") :: IO [(Int, String)]
  assembleRecipes conn results
