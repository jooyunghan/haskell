import Data.List.Split (splitOn, oneOf)
import Data.Char (isSpace)
import Data.List (elemIndex)
import Data.Maybe (fromJust)

trim :: String -> String
trim = trimLeft . trimRight

trimLeft = dropWhile isSpace
trimRight = reverse . trimLeft . reverse

lameCSV :: String -> [[String]]
lameCSV = map (map trim . splitOn ",") . splitOn "\n"

selectNames :: [[String]] -> [String]
selectNames =  map name . tail 
	where name = head

selectAges = map age . tail
	where age = (!! 1)

selectHairs = map hair . tail
	where hair = (!! 2)

select columns table = map (sequence selectors) (tail table)
	where selectors = map selector columns
	      selector = flip (!!) . indexOf
	      indexOf = fromJust . flip elemIndex header
	      header = head table

peopleData = "name, age, hair\nMerlin, 36, brown\nBob, 33, yellow"