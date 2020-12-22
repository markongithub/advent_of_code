module Day21 where

import Common
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Data.Maybe (catMaybes)
import Data.Set (Set)
import qualified Data.Set as Set

type Ingredient = String
type Allergen = String
type Label = ([Ingredient], [Allergen])
data Answer = Known Ingredient | Possibilities (Set Ingredient)
              deriving (Eq, Show)
type FullMap = Map Allergen (Set (Set Ingredient))
type ReducedMap = Map Allergen Answer

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

combineLabels :: [Label] -> FullMap
combineLabels labels = let
  insertPair :: FullMap -> (Allergen, Set Ingredient) -> FullMap
  insertPair m (key, values) = Map.insertWith Set.union key (Set.singleton values) m
  insertLabel oldMap label = foldl insertPair oldMap (
    labelToCandidateSets label)
  in foldl insertLabel Map.empty labels

mapFromInput :: [String] -> FullMap
mapFromInput input = combineLabels $ map parseLabel input

isKnown :: Answer -> Bool
isKnown (Known _) = True
isKnown _ = False

isSolved :: ReducedMap -> Bool
isSolved m = all isKnown $ Map.elems m

day21TestInput = [
    "mxmxvkd kfcds sqjhc nhms (contains dairy, fish)"
  , "trh fvjkl sbzzf mxmxvkd (contains dairy)"
  , "sqjhc fvjkl (contains soy)"
  , "sqjhc mxmxvkd sbzzf (contains fish)"]
day21FullMap = mapFromInput day21TestInput
day21ReducedMap = reduceMap day21FullMap

intersectionOfSets :: Set (Set Ingredient) -> Answer
intersectionOfSets ss =
  Possibilities $ Set.foldl Set.intersection (Set.findMin ss) ss

reduceMap :: FullMap -> ReducedMap
reduceMap m = Map.map intersectionOfSets m

deletePossibility :: Ingredient -> Answer -> Answer
deletePossibility _ (Known k) = Known k
deletePossibility i (Possibilities s) = Possibilities (Set.delete i s)

solveAllergen :: ReducedMap -> Allergen -> Ingredient -> ReducedMap
solveAllergen m a i = let
  otherAnswers = Map.withoutKeys m (Set.singleton a)
  otherSetsReduced = Map.map (deletePossibility i) otherAnswers
  in Map.insert a (Known i) otherSetsReduced

findSingleIngredient :: ReducedMap -> (Allergen, Ingredient)
findSingleIngredient m = let
  getSinglePossibility :: (Allergen, Answer) -> Maybe (Allergen, Ingredient)
  getSinglePossibility (a, (Possibilities s)) =
    if Set.size s == 1 then Just (a, Set.findMin s) else Nothing
  getSinglePossibility _ = Nothing
  maybes :: [Maybe (Allergen, Ingredient)]
  maybes = map getSinglePossibility $ Map.toList m
  in head $ catMaybes maybes

solution :: ReducedMap -> [(Allergen, Ingredient)]
solution m = let
  fromKnown (Known i) = i
  fromKnown _ = error "you called solution wrong"
  in Map.toList $ Map.map fromKnown m

solve :: ReducedMap -> [(Allergen, Ingredient)]
solve m = let
  (nextA, nextI) = findSingleIngredient m
  nextStep = solveAllergen m nextA nextI
  in case (isSolved m) of
    True  -> solution m
    False -> solve nextStep

allIngredients :: FullMap -> Set Ingredient
allIngredients m = let
  flattenSets :: Ord a => Set (Set a) -> Set a
  flattenSets ss = Set.foldl Set.union Set.empty ss
  listOfSets :: [Set Ingredient]
  listOfSets = map flattenSets $ Map.elems m
  in foldl Set.union Set.empty listOfSets

findSafeIngredients :: FullMap -> Set Ingredient
findSafeIngredients m = let
  badI = Set.fromList $ map snd $ solve $ reduceMap m
  allI = allIngredients m
  goodI = Set.difference allI badI
  in goodI

solvePart1Func :: [String] -> Int
solvePart1Func input = let
  labels = map parseLabel input
  fullMap = mapFromInput input
  safeIngredients :: [Ingredient]
  safeIngredients = Set.toList $ findSafeIngredients fullMap
  ingredientSets = map (Set.fromList . fst) labels
  howManySets i = length $ filter (Set.member i) ingredientSets
  in sum $ map howManySets safeIngredients

solvePart1 :: IO Int
solvePart1 = let
  text = readFile "data/input21.txt"
  in fmap (solvePart1Func . lines) text
