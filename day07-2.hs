import Data.Char (digitToInt)
import Data.List (group, sort, sortBy)
import Data.Ord (Down (Down), comparing)


rankHand :: String -> Int
rankHand = rank . groupHands
  where
    rank (5 : _) = 7
    rank (4 : _) = 6
    rank (3 : 2 : _) = 5
    rank (3 : _) = 4
    rank (2 : 2 : _) = 3
    rank (2 : _) = 2
    rank (1 : _) = 1
    rank _ = 0

groupHands :: String -> [Int]
groupHands hand = incHighestCard (countJacks hand) (countCards hand)

countCards :: String -> [Int]
countCards = sortBy (comparing Data.Ord.Down) . map length . group . sort . filter (/= 'J')

incHighestCard :: Int -> [Int] -> [Int]
incHighestCard amount [] = [amount]
incHighestCard amount (x:xs) = (x + amount) : xs

countJacks :: String -> Int
countJacks = length . filter (== 'J')

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
    rank 'J' = 1
    rank 'T' = 10
    rank c = digitToInt c

calcWinnings :: [(String, Int, Int)] -> Int
calcWinnings hands = sum [rank * bid | (rank, (_, _, bid)) <- zip [1 ..] hands]

main :: IO ()
main = interact $ show . calcWinnings . sortHands . map parseInput . lines
