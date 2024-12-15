import qualified Data.List.Split as Split
import qualified Data.Char as Char
import qualified Data.Maybe as Maybe
import qualified Data.Map as Map

main :: IO()
main = solve input

solve :: IO String -> IO() 
solve input = do 
    inputStr <- input
    print $ silver inputStr 
    print $ gold inputStr

silver :: String -> Integer
silver = solveMachines . parse

gold :: String -> Integer
gold = solveMachines . map addAlot . parse

type Machine = (Button, Button, Prize)
type Prize = (Integer, Integer)
type Button = (Integer, Integer)
type Pos = (Integer, Integer)
type Cache = Map.Map Pos (Maybe Integer)

addAlot :: Machine -> Machine 
addAlot (a, b, (x, y)) = (a, b, (x + 10000000000000, y + 10000000000000))

solveMachines :: [Machine] -> Integer
solveMachines = sum . Maybe.mapMaybe (snd . solveMachine Map.empty (0, 0))


solveMachine :: Cache -> Pos -> Machine -> (Cache, Maybe Integer) 
solveMachine cache pos machine@(a, b, _)
    | atPrize pos machine = (cache, Just 0)
    | not $ inBounds pos machine = (cache, Nothing)
    | Map.member pos cache = (cache, (Map.!) cache pos) 
    | otherwise = 
        let 
            (cacheA, aPress) = solveMachine cache (pressButton pos a) machine
            (cacheB, bPress) = solveMachine cacheA (pressButton pos b) machine
            val = case (aPress, bPress) of  
                (Just a, Just b) -> Just $ min (a + 3) (b + 1) 
                (Just a, Nothing) -> Just $ a + 3
                (Nothing, Just b) -> Just $ b + 1
                (Nothing, Nothing) -> Nothing
        in 
            (Map.insert pos val cacheB, val)

pressButton :: Pos -> Button -> Pos
pressButton (px, py) (bx, by) = (px + bx, py + by)

atPrize :: Pos -> Machine -> Bool 
atPrize = checkPos (==)

inBounds :: Pos -> Machine -> Bool
inBounds = checkPos (<=) 

checkPos :: (Integer -> Integer -> Bool) -> Pos -> Machine -> Bool
checkPos  f (x, y) (_, _, (prizeX, prizeY)) = f x prizeX && f y prizeY

parse :: String -> [Machine]
parse = map parseMachine . Split.splitOn "\n\n"

parseMachine :: String -> Machine
parseMachine str = 
    let 
        [ba, bb, prize] = map parseMachinePart $ lines str
    in 
        (ba, bb, prize)

parseMachinePart :: String -> Button
parseMachinePart str = 
    let 
        [x, y] = map (read . filter Char.isDigit) . filter (any Char.isDigit) $ words str
    in 
        (x, y)

input :: IO String
input = readFile "./input"

test :: IO String
test = readFile "./test"