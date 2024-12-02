import Data.List (sort)
main :: IO()
main = solve input

solve :: IO String -> IO() 
solve input = do 
    inputStr <- input
    print $ silver inputStr 
    print $ gold inputStr

silver :: String -> Int 
silver = length . filter isSafe . parseInput

gold :: String -> Int 
gold = length . filter isSafeGold . parseInput

isSafeGold :: [Int] -> Bool 
isSafeGold line = isSafe line || any (\i -> isSafe (take i line ++ drop (i + 1) line) ) [0..(length line)]

isSafe :: [Int] -> Bool 
isSafe input = 
    let 
        zipped = zipWith (-) (tail input) input
        all3Orless =  all (\n -> n > 0 && n < 4) . map abs $ zipped
        allSameDir = all (< 0) zipped || all (> 0) zipped
    in 
        allSameDir && all3Orless

parseInput :: String -> [[Int]]
parseInput = map (map read . words) . lines

input :: IO String
input = readFile "./input"

test :: IO String
test = readFile "./test"