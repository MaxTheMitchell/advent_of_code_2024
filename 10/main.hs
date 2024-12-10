import qualified Data.Set as Set

main :: IO()
main = solve input

solve :: IO String -> IO() 
solve input = do 
    inputStr <- input
    print $ silver inputStr 
    print $ gold inputStr

silver :: String -> Int
silver = solveWithFunc trailScoreSilver

gold :: String -> Int 
gold = solveWithFunc trailScoreGold

solveWithFunc :: (Grid -> Pos -> Int) -> String -> Int
solveWithFunc f str = 
    let 
        grid = parse str 
    in 
        sum . map (\(y, row) -> sum . map (\(x, _) -> f grid (x, y)) . filter ((== 0) . snd) $ zip [0..] row) $ zip [0..] grid

type Pos = (Int, Int)
type Grid = [[Int]]

trailScoreSilver :: Grid -> Pos -> Int
trailScoreSilver grid = Set.size . trailEnds grid 

trailEnds :: Grid -> Pos -> Set.Set Pos
trailEnds grid pos =
    let 
        currVal = valueAt grid pos 
    in 
        case currVal of 
            9 -> Set.singleton pos
            10 -> Set.empty
            _ -> foldl Set.union Set.empty . map (trailEnds grid) . filter ( (== currVal + 1) .  valueAt grid) $ adjs pos

trailScoreGold :: Grid -> Pos -> Int
trailScoreGold grid pos =
    let 
        currVal = valueAt grid pos 
    in 
        case currVal of 
            9 -> 1
            10 -> 0
            _ -> sum . map (trailScoreGold grid) . filter ( (== currVal + 1) .  valueAt grid) $ adjs pos

adjs :: Pos -> [Pos]
adjs (x, y) = map (\(dx, dy) -> (x + dx, y + dy)) $ zip [0, 0, -1, 1] [-1, 1, 0, 0 ] 

valueAt :: Grid -> Pos -> Int 
valueAt grid pos@(x, y)  
    | not $ inBounds pos grid = 10
    | otherwise = (grid !! y) !! x

inBounds :: Pos -> Grid -> Bool
inBounds (x, y) grid@(row:_) = x < length row && y < length grid && x >=0 && y >= 0


parse :: String -> Grid
parse = map (map (read . (: [])) ) . lines

input :: IO String
input = readFile "./input"

test :: IO String
test = readFile "./test"