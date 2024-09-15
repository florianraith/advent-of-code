import Data.Char (digitToInt)
import Data.List (group, sort, sortBy)
import Data.Ord (Down (Down), comparing)


rankHand :: String -> Int
rankHand = rank . sortBy (comparing Data.Ord.Down) . map length . group . sort
  where
    rank [5] = 7
    rank (4 : _) = 6
    rank [3, 2] = 5
    rank (3 : _) = 4
    rank (2 : 2 : _) = 3
    rank (2 : _) = 2
    rank (1 : _) = 1
    rank _ = 0

parseInput :: String -> (String, Int, Int)
parseInput = (\(hand : bid : _) -> (hand, rankHand hand, read bid)) . words

sortHands :: [(String, Int, Int)] -> [(String, Int, Int)]
sortHands = sortBy (\(handA, rankA, _) (handB, rankB, _) -> compare rankA rankB <> mconcat (zipWith compareCard handA handB))

compareCard :: Char -> Char -> Ordering
compareCard a b = compare (rank a) (rank b)
  where
    rank 'A' = 14
    rank 'K' = 13
    rank 'Q' = 12
    rank 'J' = 11
    rank 'T' = 10
    rank c = digitToInt c

calcWinnings :: [(String, Int, Int)] -> Int
calcWinnings hands = sum [rank * bid | (rank, (_, _, bid)) <- zip [1 ..] hands]

main :: IO ()
main = interact $ show . calcWinnings . sortHands . map parseInput . lines
