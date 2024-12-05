import Data.List 

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
        grid = parseInput str 
    in 
       sum $ map (\pos -> length $ filter (isValid "XMAS" grid pos) dirs) (startingPoses grid)

startingPoses :: Grid -> [Pos]
startingPoses grid = concatMap (\y -> map (\x -> (x, y)) [0..((\n -> n - 1) .length $ grid !! y)]) [0..(length grid - 1)]

gold :: String -> Int 
gold str = 
    let 
        grid = parseInput str 
    in 
        length $ filter (isValidMas grid) (startingPoses grid)

type Grid = [[Char]]
type Pos = (Int, Int)
type Dir = (Int, Int)


isValid :: String -> Grid -> Pos -> Dir -> Bool
isValid "" _ _ _ = True
isValid (c:str) grid pos dir = 
    case getCell grid pos of 
        Just cell -> cell == c && isValid str grid (updatePos pos dir) dir
        Nothing -> False

isValidMas :: Grid -> Pos -> Bool 
isValidMas grid pos =
    case getCell grid pos of 
        Just c -> c == 'A' && hasMS grid (updatePos pos (-1, -1)) (updatePos pos (1, 1)) && hasMS grid (updatePos pos (-1, 1)) (updatePos pos (1, -1))
        Nothing -> False 

hasMS :: Grid -> Pos -> Pos -> Bool 
hasMS grid posA posB =
    let 
        isMS c = c == 'M' || c == 'S'
    in  
        case (getCell grid posA, getCell grid posB) of 
            (Just a, Just b) -> isMS a && isMS b && a /= b
            _ -> False

updatePos :: Pos -> Dir -> Pos
updatePos (px, py) (dx, dy) = (px + dx, py + dy)

getCell :: Grid -> Pos -> Maybe Char 
getCell grid (x, y) = 
        safeIndex grid y >>= (`safeIndex` x)

safeIndex :: [a] -> Int -> Maybe a
safeIndex [] _ = Nothing
safeIndex (a:_) 0 = Just a
safeIndex (_:as) n = safeIndex as (n - 1) 

dirs :: [Dir]
dirs = concatMap (\x -> map (\y -> (x, y)) [-1..1]) [-1..1]

parseInput :: String -> Grid
parseInput = lines

input :: IO String
input = readFile "./input"

test :: IO String
test = readFile "./test"