import qualified Data.Map as Map
import qualified Data.Set as Set 

main :: IO()
main = solve input

solve :: IO String -> IO() 
solve input = do 
    inputStr <- input
    print $ silver inputStr 
    print $ gold inputStr

silver :: String -> Int
silver = solveWithFunc findAntiSilver

gold :: String -> Int 
gold = solveWithFunc findAnitsGold

solveWithFunc :: (Bounds -> Pos -> Pos -> Anti) -> String -> Int 
solveWithFunc f str = 
    let 
        (anteMap, bounds) = parse str
    in 
        Set.size $ Map.foldl (\allAnti -> Set.union allAnti . findAntis (f bounds)) Set.empty anteMap

type Pos = (Int, Int)
type Bounds = (Int, Int)
type Slope = (Int, Int)
type Ante = Set.Set Pos
type Anti = Set.Set Pos
type AnteMap = Map.Map Char Ante 

inBounds :: Pos -> Bounds -> Bool
inBounds (px, py) (bx, by) = px >= 0 && px < bx && py >= 0 && py < by

findAntis :: (Pos -> Pos -> Anti ) -> Ante -> Anti
findAntis f antes = 
    Set.foldl (\antis ante -> Set.foldl (\antis otherAnte -> Set.union (f ante otherAnte) antis) antis $ Set.delete ante antes) Set.empty antes

findAnitsGold :: Bounds  -> Pos  -> Pos -> Anti
findAnitsGold bounds posA posB = 
    Set.fromList . takeWhile (`inBounds` bounds) $ iterate (`updatePos` findSlope posA posB) posA

updatePos :: Pos -> Slope -> Pos 
updatePos (px, py) (sx, sy) = (px + sx, py + sy)

findSlope :: Pos -> Pos -> Slope
findSlope (xa, ya) (xb, yb) = (xa - xb, ya - yb)

findAntiSilver :: Bounds -> Pos -> Pos -> Anti 
findAntiSilver bounds pa pb = 
    let 
        anti = updatePos pa $ findSlope pa pb
    in 
        if inBounds anti bounds
            then Set.singleton anti
            else Set.empty

parse :: String -> (AnteMap, Bounds)
parse str = (findAnte str, makeBounds str)

makeBounds :: String -> Bounds
makeBounds str = (length . head $ lines str, length $ lines str)

findAnte :: String -> AnteMap
findAnte = foldl (\am (y, row) -> foldl (\am (x, c) -> Map.insertWith Set.union c (Set.singleton (x, y)) am ) am 
    . filter ((/= '.'). snd) $ zip [0..] row) Map.empty . zip [0..] .  lines

input :: IO String
input = readFile "./input"

test :: IO String
test = readFile "./test"