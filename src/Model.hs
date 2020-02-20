{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Model (Recipe, findRecipesSearch, getAllRecipes, getRecipeById) where

import Control.Monad (forM)
import Data.Aeson.Types
import Data.List (nub)
import Data.Maybe (listToMaybe)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.SqlQQ
import GHC.Generics

data Recipe = Recipe
  { name :: String
  , recipeId :: Int
  , ingredients :: [String]
  , instructions :: [String]
  , tags :: [String]
  } deriving (Eq, Generic)

instance ToJSON Recipe
instance FromJSON Recipe


getInstructions :: Connection -> Int -> IO [String]
getInstructions conn recipeId = do
  instructionResults <- query conn [sql|
      SELECT step
      FROM instruction
      WHERE recipe_id = ?
      ORDER BY step_number ASC
      |] $ Only recipeId :: IO [Only String]
  return $ map (\(Only instruction) -> instruction) instructionResults


getIngredients :: Connection -> Int -> IO [String]
getIngredients conn recipeId = do
  ingredientResults <- query conn [sql|
    SELECT amount, name
    FROM ingredient i
    INNER JOIN ingredient_type it ON i.ingredient_type_id = it.id
    WHERE recipe_id = ?
    |] $ Only recipeId
  return $ map (\(ingAmount, ingName) -> ingAmount ++ " of " ++ ingName) ingredientResults


getTags :: Connection -> Int -> IO [String]
getTags conn recipeId = do
  tagResults <- query conn [sql|
      SELECT label
      FROM recipe_tag
      WHERE recipe_id = ?
      |] $ Only recipeId
  return $ map (\(Only label) -> label) tagResults


assembleRecipes :: Connection -> [(Int, String)] -> IO [Recipe]
assembleRecipes conn recipeFragments =
  forM recipeFragments $ \(rId, rName) -> do
    instructions <- getInstructions conn rId
    ingredients <- getIngredients conn rId
    tags <- getTags conn rId
    return $ Recipe
      { name = rName
      , recipeId = rId
      , instructions = instructions
      , ingredients = ingredients
      , tags = tags
      }


getRecipeById :: Connection -> Int -> IO (Maybe Recipe)
getRecipeById conn recipeId = do
  results <- query conn [sql|
      SELECT id, name
      FROM recipe
      WHERE id = ?
      |] (Only recipeId)
  recipes <- assembleRecipes conn results
  return $ listToMaybe recipes


getAllRecipes :: Connection -> IO [Recipe]
getAllRecipes conn = do
  results <- query_ conn [sql|SELECT id, name FROM recipe|]
  assembleRecipes conn results


findRecipesByName :: Connection -> String -> IO [Recipe]
findRecipesByName conn recipeName = do
  results <- query conn [sql|
      SELECT id, name
      FROM recipe
      WHERE name LIKE ?
      |] $ Only ("%" ++ recipeName ++ "%")
  assembleRecipes conn results


findRecipesByTag :: Connection -> String -> IO [Recipe]
findRecipesByTag conn tag = do
  results <- query conn [sql|
      SELECT r.id, name
      FROM recipe_tag t
      INNER JOIN recipe r ON t.recipe_id = r.id
      WHERE label = ?
      |] (Only tag)
  assembleRecipes conn results


findRecipesSearch :: Connection -> String -> IO [Recipe]
findRecipesSearch conn search = do
  found <- (++) <$> findRecipesByName conn search <*> findRecipesByTag conn search
  return $ nub found
