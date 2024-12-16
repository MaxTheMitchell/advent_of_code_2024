import qualified Data.Set as Set
import qualified Data.List.Split as Split
import qualified Data.List as List 
import qualified Data.Maybe as Maybe
import qualified Data.List as List

main :: IO()
main = solve input

solve :: IO String -> IO() 
solve input = do 
    inputStr <- input
    print $ silver inputStr 
    print $ gold inputStr

silver :: String -> Integer
silver = boxValues . makeMoves silverFuncs . parse 

gold :: String -> Integer
gold = boxValues . makeMoves goldFuncs . widen . parse 


solvePartBoxes :: PartFuncs -> String -> Boxes
solvePartBoxes fs =  getBoxesFromState . makeMoves fs . parse 


type Move = (Integer, Integer)
type Pos = (Integer, Integer)
type Player = Pos
type Wall = Pos
type Box = Pos
type Boxes = Set.Set Box
type Walls = Set.Set Wall
type State = (Player, Walls, Boxes)
type Game = (State, [Move])

getBoxesFromState :: State -> Boxes
getBoxesFromState (_, _, boxes) = boxes

boxValues :: State -> Integer
boxValues (_, _, boxes) = sum . map boxToVal $ Set.toList boxes

boxToVal :: Pos -> Integer
boxToVal (x, y) = x + (y * 100) 

makeMoves :: PartFuncs -> Game -> State
makeMoves partFuncs (state, moves) = foldl (tryMovePlayer partFuncs) state moves

tryMovePlayer :: PartFuncs -> State -> Move -> State
tryMovePlayer pfs state@(player, _, _) = fst . tryMoveElem (pfs, movePlayer) state player

movePlayer :: OnMove
movePlayer (_, walls, boxes) _ newPos = (newPos, walls, boxes)

moveBox :: OnMove
moveBox (player, walls, boxes) oldPos newPos =  (player, walls, Set.delete oldPos $ Set.insert newPos boxes)

silverFuncs :: PartFuncs 
silverFuncs = (Set.member, hitBoxSilver, tryMoveBoxSilver)

goldFuncs :: PartFuncs 
goldFuncs = (hitWideElem, hitBoxGold, tryMoveBoxGold)

tryMoveBoxSilver :: TryMoveBox
tryMoveBoxSilver = tryMoveElem (silverFuncs, moveBox)

tryMoveBoxGold :: TryMoveBox
tryMoveBoxGold state@(_, walls, boxes) box move = 
    let 
        movedBox = addMove box move
        hitWall = boxHitWall movedBox walls
        hitBoxes = Set.delete box $ boxHitBoxes movedBox boxes
    in
        if hitWall then (state, True)
        else if Set.null hitBoxes then (moveBox state box movedBox, False)
        else 
            let 
                (newState, hitAnyWall) = foldl (\(newState, hitWall) hitBox -> if hitWall then (state, True) else tryMoveBoxGold newState hitBox move) (state, False) hitBoxes
            in 
                if hitAnyWall then (state, True)
                else (moveBox newState box movedBox, False)

type OnMove = (State -> Pos -> Pos -> State)
type HitWall = (Pos -> Walls -> Bool)
type HitBox = (Pos -> Boxes -> Maybe Box)
type TryMoveBox = State -> Box -> Move -> (State, Bool)
type PartFuncs = (HitWall, HitBox, TryMoveBox)
type FuncSuite = (PartFuncs, OnMove)

tryMoveElem :: FuncSuite -> State -> Pos -> Move -> (State, Bool)
tryMoveElem suite@((hitWall, hitBox, tryMoveBox), onMove) state@(player, walls, boxes) pos move = 
    let 
        newPos = addMove pos move
    in 
        if hitWall newPos walls then (state, True)
        else case hitBox newPos boxes of 
            Nothing ->  (onMove state pos newPos, False)
            Just box -> 
                let 
                    (newState, hitWall) = tryMoveBox state box move
                in 
                    if hitWall then (state, True)
                    else tryMoveElem suite newState pos move


wideBoxHitElem :: Pos -> Set.Set Pos -> Bool
wideBoxHitElem pos set = hitWideElem pos set || hitWideElem (addXPos pos) set

boxHitWall :: Box -> Walls -> Bool 
boxHitWall box walls =  hitWideElem box walls || hitWideElem (addXPos box) walls

hitWideElem :: Pos -> Set.Set Pos -> Bool
hitWideElem pos = any (\otherPos -> pos == otherPos || pos == addXPos otherPos)

boxHitBoxes :: Box -> Boxes -> Boxes
boxHitBoxes box boxes = Set.fromList  $ Maybe.mapMaybe (`hitBoxGold` boxes) [box, addXPos box]

hitBoxGold :: Pos -> Boxes -> Maybe Box
hitBoxGold pos boxes
    | Set.member pos boxes = Just pos
    | Set.member (subXPos pos) boxes = Just $ subXPos pos
    | otherwise = Nothing

hitBoxSilver :: Pos -> Boxes -> Maybe Box
hitBoxSilver pos boxes 
    | Set.member pos boxes = Just pos
    | otherwise = Nothing

addXPos :: Pos -> Pos
addXPos = addMove (1, 0)

subXPos :: Pos -> Pos
subXPos = addMove (-1, 0)

addMove :: Pos -> Move -> Pos
addMove (px, py) (mx, my) = (px + mx, py + my)

parse :: String -> Game
parse str = 
    let 
        [stateStr, movesStr] = Split.splitOn "\n\n" str
    in 
        (parseState stateStr, parseMoves movesStr)

widen :: Game -> Game
widen ((player, walls, boxes), moves) = 
    let 
        addXPos (x, y) = (x * 2, y)
    in 
        ((addXPos player, Set.map addXPos walls, Set.map addXPos boxes), moves)

parseState :: String -> State
parseState = foldl (\state (y, row) -> foldl (\state (x, c) -> addToState state (x, y) c )  state $ zip [0..] row ) initState . zip [0..] . lines

addToState :: State -> Pos -> Char -> State
addToState state@(player, walls, boxes) pos c =
    case c of 
        '@' -> (pos, walls, boxes)
        '#' -> (player, Set.insert pos walls, boxes) 
        'O' -> (player, walls, Set.insert pos boxes)
        _ -> state

initState :: State 
initState = ((0,0), Set.empty, Set.empty)

parseMoves :: String -> [Move]
parseMoves = Maybe.mapMaybe charToMove

charToMove :: Char -> Maybe Move
charToMove '<' = Just (-1, 0)
charToMove '>' = Just (1, 0)
charToMove '^' = Just (0, -1)
charToMove 'v' = Just (0, 1)
charToMove _ = Nothing

input :: IO String
input = readFile "./input"

test :: IO String
test = readFile "./test"
