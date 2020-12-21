module Day21 where

import Common
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

type Ingredient = String
type Allergen = String
type Label = ([Ingredient], [Allergen])
type IngredientMap = Map Allergen (Set (Set Ingredient))

parseLabel :: String -> Label
parseLabel s = let
  ingredientsStr = init $ takeWhile (/= '(') s
  ingredients = splitOnSeparator ' ' ingredientsStr
  allergensStr = init $ drop (length ingredientsStr + 11) s
  allergens = splitOnSeparator ',' $ filter (/= ' ') allergensStr
  in (ingredients, allergens)
 
labelToCandidateSets :: Label -> [(Allergen, Set Ingredient)]
labelToCandidateSets (ingredients, allergens) = let
  makePair a = (a, Set.fromList ingredients)
  in map makePair allergens

combineLabels :: [Label] -> IngredientMap
combineLabels labels = let
  insertPair :: IngredientMap -> (Allergen, Set Ingredient) -> IngredientMap
  insertPair m (key, values) = Map.insertWith Set.union key (Set.singleton values) m
  insertLabel oldMap label = foldl insertPair oldMap (
    labelToCandidateSets label)
  in foldl insertLabel Map.empty labels

mapFromInput :: [String] -> IngredientMap
mapFromInput input = combineLabels $ map parseLabel input

isSolved :: IngredientMap -> Bool
isSolved m = let
  allOneSet = all (== 1) $ map Set.size $ Map.elems m
  allOneIngredient = all (== 1) $ map (Set.size . Set.findMin) $ Map.elems m
  in allOneSet && allOneIngredient

day21TestInput = [
    "mxmxvkd kfcds sqjhc nhms (contains dairy, fish)"
  , "trh fvjkl sbzzf mxmxvkd (contains dairy)"
  , "sqjhc fvjkl (contains soy)"
  , "sqjhc mxmxvkd sbzzf (contains fish)"]
day21TestMap = mapFromInput day21TestInput

