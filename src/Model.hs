{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Model (Recipe, findRecipesSearch, getAllRecipes, getRecipeById) where

import Control.Monad (forM)
import Data.Aeson.Types
import Data.List (nub)
import Data.Maybe (listToMaybe)
import Database.PostgreSQL.Simple
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
  instructionResults <- query conn "SELECT step FROM instruction WHERE recipe_id = ? ORDER BY step_number ASC" $ Only recipeId :: IO [Only String]
  return $ map (\(Only instruction) -> instruction) instructionResults


getIngredients :: Connection -> Int -> IO [String]
getIngredients conn recipeId = do
  ingredientResults <- query conn
    "SELECT amount, name FROM ingredient i INNER JOIN ingredient_type it ON i.ingredient_type_id = it.id WHERE recipe_id = ?" $ Only recipeId
  return $ map (\(ingAmount, ingName) -> ingAmount ++ " of " ++ ingName) ingredientResults


getTags :: Connection -> Int -> IO [String]
getTags conn recipeId = do
  tagResults <- query conn "select label from recipe_tag where recipe_id = ?" $ Only recipeId
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


findRecipesByTag :: Connection -> String -> IO [Recipe]
findRecipesByTag conn tag = do
  results <- query conn "select r.id, name from recipe_tag t inner join recipe r on t.recipe_id = r.id where label = ?" $ Only tag
  assembleRecipes conn results


findRecipesSearch :: Connection -> String -> IO [Recipe]
findRecipesSearch conn search = do
  found <- (++) <$> findRecipesByName conn search <*> findRecipesByTag conn search
  return $ nub found
