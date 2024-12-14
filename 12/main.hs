import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Maybe as Maybe

main :: IO()
main = solve input

solve :: IO String -> IO() 
solve input = do 
    inputStr <- input
    print $ silver inputStr 
    print $ gold inputStr

silver :: String -> Int
silver = sum . map (regionPrice getTotalPeram) . createRegions . parse

gold :: String -> Int
gold = sum . map (regionPrice getSides) . createRegions . parse

type Pos = (Int, Int)
type Edge = (Float, Float, Bool)
type Grid a = [[a]]
type Region = Set.Set Pos

createRegions :: Grid Char -> [Region]
createRegions grid =  
    let 
        createRegionsInner ::  Set.Set Pos -> [Region]
        createRegionsInner poses
            | Set.null poses = []
            | otherwise = 
                let 
                    region = createRegion Set.empty grid (Set.findMax poses) 
                in 
                    region : createRegionsInner (Set.difference poses region)
    in 
        createRegionsInner $ gridPoses grid

regionPrice :: (Region -> Int) -> Region -> Int 
regionPrice  f reg = Set.size reg * f reg

createRegion :: Region -> Grid Char -> Pos -> Region
createRegion reg grid pos = 
    let 
        newReg = Set.insert pos reg 
    in 
         foldl (`createRegion` grid ) newReg  . filter (\adj -> (valueAt grid adj == valueAt grid pos) && Set.notMember adj newReg) $ adjs pos

getSides :: Region -> Int 
getSides = length . groupEdges . map Set.singleton . getEdges

groupEdges :: [Set.Set Edge] -> [Set.Set Edge]
groupEdges edges =
    let 
        newEdges = tryGroupEdges edges
    in 
        if length edges == length newEdges
            then newEdges
            else groupEdges newEdges

tryGroupEdges :: [Set.Set Edge] -> [Set.Set Edge]
tryGroupEdges [] = []
tryGroupEdges old@(x:xs) = 
    let 
        new = tryCombEdge x xs
    in 
        if length new == length old 
            then x: tryGroupEdges xs
            else tryGroupEdges new

tryCombEdge :: Set.Set Edge -> [Set.Set Edge] -> [Set.Set Edge]
tryCombEdge es [] = [es]
tryCombEdge es (x:xs) 
    | any (\edge -> any (edgesAdj edge) x) es = Set.union es x : xs
    | otherwise = x : tryCombEdge es xs


getEdges :: Region -> [Edge]
getEdges reg = concatMap (\pos -> map (toEdge pos) . filter (`Set.notMember` reg) $ adjs pos) reg  

toEdge :: Pos -> Pos -> Edge
toEdge (xa, ya) (xb, yb) =
    let 
        dx = fromIntegral (xb - xa) / 2
        dy = fromIntegral (yb - ya) / 2
    in 
        (fromIntegral xa + dx,  fromIntegral ya + dy, xa >= xb && ya >= yb)

edgesAdj :: Edge -> Edge -> Bool 
edgesAdj (xa, ya, dirA) (xb, yb, dirB) = 
      let 
          oneDiff a b = abs (a - b) == 1
          good a b = isInt a && isInt b && oneDiff a b
      in
          ((good xa xb && ya == yb) || (good ya yb && xa == xb)) && dirA == dirB

isInt :: Float -> Bool 
isInt a = a == fromInteger (round a)

getTotalPeram :: Region -> Int 
getTotalPeram reg = foldl (\sum -> (+ sum) . getPeram reg) 0 reg

getPeram :: Region -> Pos -> Int
getPeram reg  = length . filter (`Set.notMember` reg ) . adjs

adjVals :: Grid a -> Pos -> [Maybe a]
adjVals grid = map (valueAt grid) . adjs

adjs :: Num a => (a, a) -> [(a, a)]
adjs (x, y) = map (\(dx, dy) -> (x + dx, y + dy)) $ zip [0, 0, -1, 1] [-1, 1, 0, 0 ] 

valueAt ::  Grid a -> Pos -> Maybe a 
valueAt grid pos@(x, y) 
    | not $ inBounds pos grid = Nothing
    | otherwise = Just $ (grid !! y) !! x

inBounds :: Pos -> Grid a -> Bool
inBounds (x, y) grid@(row:_) = x < length row && y < length grid && x >=0 && y >= 0

gridPoses :: Grid a -> Set.Set Pos
gridPoses = Set.fromList . concatMap (\(y, row) -> map (\(x, _) -> (x, y)) $ zip [0..] row) . zip [0..]

parse :: String -> Grid Char
parse = lines

input :: IO String
input = readFile "./input"

test :: IO String
test = readFile "./test"