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

addAlot :: Machine -> Machine 
addAlot (a, b, (x, y)) = (a, b, (x + 10000000000000, y + 10000000000000))

solveMachines :: [Machine] -> Integer
solveMachines = sum . Maybe.mapMaybe solveMatchineMathly

solveMatchineMathly :: Machine -> Maybe Integer
solveMatchineMathly machine = do
    bPresses <- findBPresses machine
    aPresses <- findAPresses machine bPresses
    return $ aPresses * 3 + bPresses

findAPresses :: Machine -> Integer -> Maybe Integer 
findAPresses ((ax, ay), (bx, by), (px, py)) bPresses =
    maybeInt $ fromInteger (px - bx*bPresses) / fromInteger ax 
    --  px = ax*pressA + bx*pressB
    -- px - bx*pressB = ax*pressA
    -- (px - bx*pressB)/ax = PressA 

findBPresses :: Machine -> Maybe Integer
findBPresses ((ax, ay), (bx, by), (px, py)) = 
    maybeInt $ fromInteger (py*ax - px*ay) / fromInteger (by*ax - bx*ay)
    --  px = ax*pressA + bx*pressB
    -- px - bx*pressB = ax*pressA
    -- (px/ax) - ((bx*pressB)/ax) = pressA

    -- (px/ax) - ((bx*pressB)/ax) = (py/ay) - ((by*pressB)/ay)
    -- (px*ay) - (bx*pressB*ay) = (py*ax) - (by*pressB*ax)
    -- (by*pressB*ax) - (bx*pressB*ay) = (py*ax) - (px*ay)
    -- pressB(by*ax - bx*ay) = (py*ax) - (px*ay)
    -- pressB = (py*ax) - (px*ay) / (by*ax - bx*ay)

maybeInt :: (RealFrac n) => n -> Maybe Integer
maybeInt n
    | isInt n = Just $ round n
    | otherwise = Nothing

isInt :: (RealFrac n) => n -> Bool 
isInt n = n == fromInteger (round n) 

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