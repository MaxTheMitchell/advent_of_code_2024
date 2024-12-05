import qualified Data.Map as Map
import Data.List (find, intersect, delete)
import Data.List.Split (splitOn)

main :: IO()
main = solve input

solve :: IO String -> IO() 
solve input = do 
    inputStr <- input
    print $ silver inputStr 
    print $ gold inputStr

silver :: String -> Int 
silver str = 
    let 
        (mapping, lsts) = parse str 
    in 
        sum . map midLst $ filter (validLst mapping) lsts

gold :: String -> Int 
gold str = 
    let 
        (mapping, lsts) = parse str 
    in 
        sum . map (midLst . orderLst mapping) $ filter (not . validLst mapping) lsts

type Before = Int 
type After = Int 
type Mapping = Map.Map Before [After] 

orderLst :: Mapping -> [Int] -> [Int]
orderLst _ [] = []
orderLst mapping lst = 
    let 
        num = find (\k -> null . intersect lst $ Map.findWithDefault [] k mapping) lst 
    in
        case num of 
            Just num -> num : orderLst mapping (delete num lst)
            Nothing -> []

midLst :: [Int] -> Int 
midLst lst = lst !! (length lst `div` 2) 

validLst :: Mapping -> [Int] -> Bool 
validLst _ [] = True 
validLst mapping (x:xs) =
     notElem x (concatMap (\k -> Map.findWithDefault [] k mapping) xs)
        && validLst mapping xs

parse :: String -> (Mapping, [[Int]])
parse str = 
    let 
        [mappingStr, lstStr] = splitOn "\n\n" str 
    in 
        (parseMapping mappingStr, parseLsts lstStr)

parseMapping :: String -> Mapping 
parseMapping = 
    foldl insertPairInMap Map.empty
    . map ((\[a, b] -> (a, b)) . map read . splitOn "|") . lines

insertPairInMap :: Mapping -> (Int, Int) -> Mapping
insertPairInMap mapping (start, end) = 
    Map.insertWith (++) start [end] mapping 

parseLsts :: String -> [[Int]] 
parseLsts = map (map read . splitOn ",") . lines

input :: IO String
input = readFile "./input"

test :: IO String
test = readFile "./test"