splitOn :: Char -> String -> [String]
splitOn d s = case dropWhile (== d) s of
  "" -> []
  s' -> w : splitOn d s''
    where
      (w, s'') = break (== d) s'

parseCard :: String -> [[Int]]
parseCard = map (map read . filter (/= "") . splitOn ' ') . splitOn '|' . last . splitOn ':'

winningNumbers :: [[Int]] -> [Int]
winningNumbers [wins, nums] = filter (`elem` wins) nums

points :: [Int] -> Int
points [] = 0
points x = 2 ^ (length x - 1)

main :: IO ()
main = interact $ show . sum . map (points . winningNumbers . parseCard) . lines
