import qualified Data.Set as Set
import qualified Data.Foldable as Set
-- import Data.List (find, intersect, delete)

main :: IO()
main = solve input

solve :: IO String -> IO() 
solve input = do 
    inputStr <- input
    print $ silver inputStr 
    print $ gold inputStr

silver :: String -> Int 
silver = Set.length . Set.fromList . play . parse

gold :: String -> Int 
gold = length . filter (gameLoops Set.empty) . plusOneBlockPerms . parse 

type Pos = (Int, Int)
type Blocks = Set.Set Pos
type Dir = (Int, Int)
type Limits = (Int, Int)
type Player = (Pos, Dir)
type Game = (Player, Blocks, Limits)

plusOneBlockPerms :: Game -> [Game] 
plusOneBlockPerms (player@(pos, _), blocks, limits@(lx, ly)) =
    map (\block -> (player, Set.insert block blocks, limits)). filter ((/= pos)) $ concatMap (\x -> map (\y ->  (x, y)) [0..ly]) [0..lx]

gameLoops :: Set.Set Player -> Game -> Bool
gameLoops cache game@(player, blocks, limits)
    | outOfBounds game = False 
    | Set.member player cache = True
    | otherwise = gameLoops (Set.insert player cache) (tryMovePlayer player blocks, blocks, limits)

play :: Game -> [Pos]
play game@(player@(pos, _), blocks, limits) 
    | outOfBounds game = []
    | otherwise = 
        pos: play (tryMovePlayer player blocks, blocks, limits)

tryMovePlayer :: Player -> Blocks -> Player 
tryMovePlayer orginalPlayer blocks= 
    let 
        newPlayer = movePlayer orginalPlayer
    in 
        if hitBlock newPlayer blocks 
            then tryMovePlayer (rotatePlayer orginalPlayer) blocks
            else newPlayer

hitBlock :: Player -> Blocks -> Bool
hitBlock (pos, _) = Set.member pos 

outOfBounds :: Game -> Bool 
outOfBounds (((x, y), _), _, (lx, ly)) = x < 0 || x >= lx || y < 0 || y >= ly

rotatePlayer :: Player -> Player
rotatePlayer (pos, (dx, dy)) = (pos, (-dy, dx))

movePlayer :: Player -> Player
movePlayer ((x, y), dir@(dx, dy)) = ((x + dx, y + dy), dir)

parse :: String -> Game
parse str = (parsePlayer str, parseBlocks str, (length . head $ lines str, length $ lines str))

parsePlayer :: String -> Player
parsePlayer str = (head $ getPosesOfChar '^' str, (0, -1))

parseBlocks :: String -> Set.Set Pos
parseBlocks = Set.fromList . getPosesOfChar '#'

getPosesOfChar :: Char -> String -> [Pos]
getPosesOfChar char = concatMap (\(y, row) -> 
    map (\(x, _) -> (x, y)) . filter ((== char) . snd) $ zip [0..] row) . zip [0..] . lines

input :: IO String
input = readFile "./input"

test :: IO String
test = readFile "./test"