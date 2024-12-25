import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.List.Split as Split
import qualified Data.List as List
import Data.Function (on)


main :: IO()
main = solve input

solve :: IO String -> IO() 
solve input = do 
    inputStr <- input
    print $ silver inputStr 
    print $ gold inputStr

silver :: String -> Int
silver = length . Set.filter goodSilverCon . getAllCons 3 . findConnections .  parse

gold :: String -> String
gold = lanNetToStr . biggestLan . findConnections . parse

input :: IO String
input = readFile "./input"

test :: IO String
test = readFile "./test"

type Network = Map.Map  String (Set.Set String)
type LanNetwork = Set.Set String
type Connection = (String, String)

lanNetToStr :: LanNetwork -> String
lanNetToStr = List.intercalate "," . List.sort . Set.toList

biggestLan :: Network -> LanNetwork 
biggestLan net = List.maximumBy (compare `on` Set.size) . Set.toList $ connectedLans net Set.empty Set.empty

connectedLans :: Network -> LanNetwork -> Set.Set LanNetwork -> Set.Set LanNetwork
connectedLans net lan checked 
    | Set.member lan checked = checked
    | otherwise =
        foldl (\checked key -> connectedLans net (Set.insert key lan) checked) (Set.insert lan checked)
        . filter (canAddToLan net lan) 
        . Set.toList 
        $ Set.difference (Map.keysSet net) lan

canAddToLan :: Network -> LanNetwork -> String -> Bool
canAddToLan net lan key =  lan `Set.isSubsetOf` Set.insert key (net Map.! key)  

goodSilverCon :: Set.Set String -> Bool
goodSilverCon set = Set.size set == 3 && any ((== 't') . head) set  

getAllCons :: Int -> Network -> Set.Set (Set.Set String)
getAllCons depth net = foldl1 Set.union . Set.map (\k -> getCons depth k k net) $ Map.keysSet net

getCons :: Int -> String ->  String -> Network  -> Set.Set (Set.Set String)
getCons 0 target key _ = if target == key then Set.singleton Set.empty else Set.empty 
getCons depth target key net = Set.map (Set.insert key) . foldl1 Set.union . Set.map (\k -> getCons (depth - 1) target k net) $ net Map.! key   

findConnections :: [Connection] -> Network
findConnections = foldl addConnection Map.empty

addConnection :: Network -> Connection -> Network
addConnection net (a, b) =
    let 
        addToNet a b = Map.insertWith Set.union a (Set.singleton b)
    in
        addToNet b a $ addToNet a b net

parse :: String -> [Connection]
parse = map parseLine . lines

parseLine :: String -> Connection
parseLine str = let 
    [a, b] = Split.splitOn "-" str
    in 
        (a, b)