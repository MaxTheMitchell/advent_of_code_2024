import qualified Data.List.Split as Split
import qualified Data.List as List
import qualified Data.Char as Char
import qualified Data.Maybe as Maybe
import qualified Data.Foldable as List

main :: IO()
main = solve input

solve :: IO (String, Bounds) -> IO() 
solve input = do 
    (inputStr, bounds) <- input
    print $ silver bounds inputStr 
    gold bounds inputStr

silver :: Bounds -> String -> Int
silver b = product . map length . List.group . List.sort . Maybe.mapMaybe (inQuad b) . moveN 100 b . parse

gold ::  Bounds -> String -> IO()
gold b str = do 
        putStrLn . List.intercalate "\n\n" . take 200 . filter shouldDisplay . map (\(i, robots) -> show i ++ "\n" ++ makeDisplay b robots) .  iterate (\(i, robots) -> (i + 1, moveAll b robots)) $  (0, parse str)
        return ()

type Pos = (Int, Int)
type Vel = (Int, Int)
type Bounds = (Int, Int)
type Robot = (Pos, Vel)
type Quad = (Bool, Bool)

makeDisplay :: Bounds -> [Robot] -> String 
makeDisplay b robots = 
    List.intercalate "\n" .
    map (map (\p -> if inRobots p robots then '█' else '░')) $ makeGrid b

shouldDisplay :: String -> Bool
shouldDisplay = List.isInfixOf "█████"


inRobots :: Pos -> [Robot] -> Bool
inRobots p = List.elem p . map fst 

makeGrid :: Bounds -> [[Pos]]
makeGrid (bx, by) = map (\y -> map (\x -> (x, y)) [0..by] ) [0..bx]

inQuad :: Bounds -> Robot -> Maybe Quad
inQuad (bx, by) ((rx, ry), _) =
    let 
        blx = (bx - 1) `div` 2
        bly = (by - 1) `div` 2
    in
        if blx == rx || bly == ry 
            then Nothing 
            else Just (blx < rx, bly < ry)

moveN :: Int -> Bounds -> [Robot] -> [Robot]
moveN n b rob = foldl (\rob _ -> moveAll b rob) rob [0..n - 1]

moveAll :: Bounds -> [Robot] -> [Robot]
moveAll b = map (move b)

move :: Bounds -> Robot -> Robot
move (bx, by) ((rx, ry), vol@(vx, vy)) =
    let
        moveAxis b r v = 
            let p = (r + v) `mod` b in 
                if p < 0 then b - p else p 
    in
        ((moveAxis bx rx vx, moveAxis by ry vy), vol)

parse :: String -> [Robot]
parse = map parseLine . lines

parseLine :: String -> Robot
parseLine str = 
    let 
        [px, py, vx, vy] = map (read . filter (\c -> Char.isDigit c || c == '-')) $ Split.splitOneOf " ," str
    in
        ((px, py), (vx, vy))

input :: IO (String, Bounds)
input = do 
    str <- readFile "./input" 
    return (str, (101, 103))

test :: IO (String, Bounds)
test = do 
    str <- readFile "./test" 
    return (str, (11, 7))