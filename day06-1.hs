splitOn :: (Eq a) => a -> [a] -> [[a]]
splitOn d s = case dropWhile (== d) s of
  [] -> []
  s' -> w : splitOn d s''
    where
      (w, s'') = break (== d) s'

parse :: [String] -> [(Int, Int)]
parse = uncurry zip . (\[x, y] -> (x, y)) . map (map read . words . last . splitOn ':')

wins :: (Int, Int) -> Int
wins (t, rec) = length $ filter (> rec) $ map (\x -> x * (t - x)) [0 .. t]

main :: IO ()
main = interact $ show . product . map wins . parse . lines
