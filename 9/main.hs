import Data.List (intersperse, find)
import Data.Char (isDigit)

main :: IO()
main = solve input

solve :: IO String -> IO() 
solve input = do 
    inputStr <- input
    print $ silver inputStr 
    print $ gold inputStr

silver :: String -> Int
silver = checkSum . fillMemory . parse

gold :: String -> Int 
gold = checkSum . memGoldToNums  . fillMemoryGold . parseGold

type Index = Int
type Memory = Maybe Index 
type MemoryGold = (Index, Int, Bool)

checkSum :: [Int] -> Int 
checkSum = sum . zipWith (*) [0..]

fillMemory :: [Memory] -> [Int]
fillMemory [] = []
fillMemory lst@(x:xs) = case x of 
    Just x -> x : fillMemory xs
    Nothing -> case last xs of 
        Just x -> x : fillMemory (init xs)
        Nothing -> fillMemory (init lst)

memGoldToNums :: [MemoryGold] -> [Int]
memGoldToNums [] = []
memGoldToNums ((i, num, _):xs) =
   replicate num (if i == -1 then 0 else i) ++ memGoldToNums xs

fillMemoryGold :: [MemoryGold] -> [MemoryGold]
fillMemoryGold mem =
    case find (\(_, _, checked) -> not checked) $ reverse mem of 
        Nothing -> mem
        Just toCheck@(i, _, _) -> 
            let
                before = takeWhile (\(thisI, _, _) -> i /= thisI) mem
                after = tail $ dropWhile (\(thisI, _, _) -> i /= thisI) mem
            in 
                fillMemoryGold (tryInsert before toCheck ++ after)

tryInsert :: [MemoryGold] -> MemoryGold -> [MemoryGold]
tryInsert [] (i, size, _) = [(i, size, True)]
tryInsert (x@(chunkI, chunkSize, _):xs) memInsert@(i, size, _) = 
    if chunkI == -1 && chunkSize >= size 
        then (i, size, True) : (-1, chunkSize - size, True) : xs ++ [(-1, size, True)] 
        else x : tryInsert xs memInsert
    
parseGold :: String -> [MemoryGold]
parseGold = toMemoryGold True 0 . toInts

parse :: String -> [Memory]
parse =  toMemory True 0 . toInts

toInts :: String -> [Int]
toInts = map (\c -> read [c]) . filter isDigit

toMemory :: Bool -> Int -> [Int] -> [Memory]
toMemory _ _ [] = []
toMemory fill i (x:xs) =
    replicate x (if fill then Just i else Nothing) ++ toMemory (not fill) (if fill then i + 1 else i) xs

toMemoryGold :: Bool -> Int -> [Int] -> [MemoryGold]
toMemoryGold _ _ [] = []
toMemoryGold fill i (x:xs) =
    (if fill then i else -1, x, not fill) : toMemoryGold (not fill) (if fill then i + 1 else i) xs

input :: IO String
input = readFile "./input"

test :: IO String
test = readFile "./test"