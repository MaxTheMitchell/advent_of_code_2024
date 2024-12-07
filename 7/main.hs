import Data.List.Split (splitOn)

main :: IO()
main = solve input

solve :: IO String -> IO() 
solve input = do 
    inputStr <- input
    print $ silver inputStr 
    print $ gold inputStr

silver :: String -> Integer 
silver = solveWithOpts silverOpts

gold :: String -> Integer 
gold = solveWithOpts goldOpts

type Opt = Integer -> Integer -> Integer

solveWithOpts :: [Opt] -> String -> Integer
solveWithOpts opts = sum . map fst . filter (\(x, xs) -> elem x $ combos opts xs) . parse

silverOpts :: [Opt]
silverOpts = [(+), (*)]

goldOpts :: [Opt]
goldOpts = [(+), (*), concatOp]

concatOp :: Integer -> Integer -> Integer
concatOp a b = a * ((^) 10 . (1 +) . floor . logBase 10 $ fromIntegral b) + b

combos :: [Opt] -> [Integer] -> [Integer]
combos opts (x:xs) = foldl (\nums num -> concatMap (\opt -> map (`opt` num) nums) opts) [x] xs

parse :: String -> [(Integer, [Integer])]
parse = map parseLine . lines

parseLine :: String -> (Integer, [Integer])
parseLine str = 
    let 
        [target, nums] =  splitOn ":" str
    in 
        (read target, map read $ words nums)

input :: IO String
input = readFile "./input"

test :: IO String
test = readFile "./test"