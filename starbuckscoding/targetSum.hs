import Data.List (group, sort, tails)
import Data.Maybe (catMaybes)

targetSum ints target = 
  targetSubSets (group (sort ints)) target

targetSubSets :: [[Int]] -> Int -> [[Int]]
targetSubSets _ 0 = [[]]
targetSubSets [] target = []
targetSubSets (g:gs) target = 
  [p ++ s| p <- tails g, (target - sum p) >= 0, s <- targetSubSets gs (target - sum p)]
