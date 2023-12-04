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

newCopies :: [Int] -> Int -> Int -> [Int]
newCopies copies card wins = take maxLength $ zipWith (+) paddedCopies $ replicate card 0 ++ replicate wins turns ++ repeat 0
  where
    maxLength = max (length copies) (card + wins)
    paddedCopies = copies ++ repeat 0
    turns = 1 + paddedCopies !! (card - 1)

totalCopies :: [Int] -> Int -> [Int] -> [Int]
totalCopies copies card games
  | card - 1 == length games = copies
  | otherwise = totalCopies (newCopies copies card (games !! (card - 1))) (card + 1) games

totalCards :: [Int] -> [Int]
totalCards = map (+ 1) . totalCopies [0] 1

main :: IO ()
main = interact $ show . sum . totalCards . map (length . winningNumbers . parseCard) . lines
