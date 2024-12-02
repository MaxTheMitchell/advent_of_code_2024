import Data.List (sort)
main :: IO ()
main = do
    inputStr <- input
    print $ silver inputStr 
    print $ gold inputStr

silver :: String -> Int 
silver str = 
    let 
        (line1, line2) = parseInput str 
    in
        sum . map abs $ zipWith (-) (sort line1) (sort line2)

gold :: String -> Int
gold str = 
    let 
        (line1, line2) = parseInput str
    in 
        sum $ map (\n ->  (* n) . length $ filter (== n) line2) line1

parseInput :: String -> ([Int], [Int])
parseInput str =
    let 
        pLines = parseLines str
    in 
        (map fst pLines, map snd pLines)

parseLines :: String -> [(Int, Int)]
parseLines = map parseLine . lines

parseLine :: String -> (Int, Int)
parseLine str = 
    let 
        [a, b] = map read (words str)
    in 
        (a, b)

input :: IO String
input = readFile "./input"

test :: IO String 
test = readFile "./test"