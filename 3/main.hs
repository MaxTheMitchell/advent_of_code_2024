import Data.List (stripPrefix, isPrefixOf)
import Data.List.Split (splitOn)
import Data.Char (isDigit)

main :: IO()
main = solve input

solve :: IO String -> IO()
solve input = do
    inputStr <- input
    print $ silver inputStr
    print $ gold inputStr True

gold :: String -> Bool -> Int
gold "" _ = 0
gold str active 
  | "do()" `isPrefixOf` str = gold (tail str) True
  | "don't()" `isPrefixOf` str = gold (tail str) False
  | active = tryMulNums str + gold (tail str) True
  | otherwise = gold (tail str) False 

silver :: String -> Int
silver "" = 0
silver str = tryMulNums str + silver (tail str)

tryMulNums :: String -> Int 
tryMulNums str =  let
        newStr = stripPrefix "mul(" str
    in
        case newStr of
            Nothing -> 0
            Just newStr -> tryParseNums (takeWhile (/= ')') newStr) 

tryParseNums :: String -> Int
tryParseNums str =
    let
        arr = splitOn "," str
    in
        if length arr /= 2
            then 0
            else
                (\[a, b]-> case (parseNum a, parseNum b) of
                    (Just na, Just nb) -> na * nb
                    _ -> 0
                )  arr


parseNum :: String -> Maybe Int
parseNum str =
    if all isDigit str
        then Just $ read str
        else Nothing

input :: IO String
input = readFile "./input"

test :: IO String
test = readFile "./test"