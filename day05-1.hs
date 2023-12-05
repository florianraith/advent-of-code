import Data.Maybe (mapMaybe)
import Debug.Trace (trace)

type Range = (Int, Int, Int)
type Map = [Range]

splitOn :: (Eq a) => a -> [a] -> [[a]]
splitOn d s = case dropWhile (== d) s of
  [] -> []
  s' -> w : splitOn d s''
    where
      (w, s'') = break (== d) s'

seeds :: [String] -> [Int]
seeds = map read . filter (/= "") . splitOn ' ' . last . splitOn ':' . head

maps :: [String] -> [Map]
maps = map (map ((\[x1, x2, x3] -> (x1, x2, x3)) . map read . splitOn ' ') . tail) . splitOn "" . drop 2

convert :: Int -> Range -> Maybe Int
convert x (dst, src, len)
  | x >= src && x < src + len = Just (dst + x - src)
  | otherwise = Nothing

applyMap :: Map -> Int -> Int
applyMap m x = head $ mapMaybe (convert x) m ++ [x]

locations :: [Int] -> [Map] -> [Int]
locations = foldl (\acc m -> map (applyMap m) acc)

main :: IO ()
main = interact $ show . minimum . (\input -> locations (seeds input) (maps input)) . lines
