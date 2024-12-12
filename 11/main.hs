import qualified Data.Map as Map

main :: IO()
main = solve input

solve :: IO String -> IO() 
solve input = do 
    inputStr <- input
    print $ silver inputStr 
    print $ gold inputStr

silver :: String -> Integer
silver = blinkNtimes 25 . parse

gold :: String -> Integer 
gold = blinkNtimes 75 . parse

type Cache = Map.Map (Integer, Integer) Integer

blinkNtimes :: Integer -> [Integer] -> Integer
blinkNtimes n = fst . foldl (\(total, cache) stone -> (\(stoneVal, newCache) -> (total + stoneVal, newCache)) $ cachedStoneBlink cache n stone) (0, Map.empty)

blinkStone :: Integer -> [Integer]
blinkStone stone 
    | stone == 0 = [1]
    | length (show stone) `mod` 2 == 1 = [stone * 2024]
    | otherwise = 
        let 
            stoneStr = show stone
            half = length stoneStr `div` 2
        in 
            map read [
                take half stoneStr,
                drop half stoneStr
            ]

cachedStoneBlink :: Cache -> Integer -> Integer -> (Integer, Cache)
cachedStoneBlink cache 0 _  = (1, cache)
cachedStoneBlink cache i stone = 
    case Map.lookup (i, stone) cache of 
        Just val -> (val, cache) 
        Nothing -> 
            let 
              (total, newCache) =  foldl (\(total, newCache) newStone ->
                  (\(stoneVal, newCache) -> (total + stoneVal, newCache)) $ 
                  cachedStoneBlink newCache (i - 1) newStone) 
                  (0, cache) $ blinkStone stone
            in
                (total, Map.insert (i, stone) total newCache)

parse :: String -> [Integer]
parse = map read . words

input :: IO String
input = readFile "./input"

test :: IO String
test = readFile "./test"