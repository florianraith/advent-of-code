import Data.Char

type IInt = (Int, [Int]) -- index int

numbers :: String -> [IInt]
numbers = numAcc 0 [] (0, [])
  where
    numAcc :: Int -> [IInt] -> IInt -> String -> [IInt]
    numAcc _ acc (0, _) "" = acc
    numAcc _ acc partialNum "" = acc ++ [partialNum]
    numAcc i acc (partialNum, indices) (x : xs)
      | isDigit x = numAcc (i + 1) acc (10 * partialNum + digitToInt x, indices ++ [i]) xs
      | partialNum /= 0 = numAcc (i + 1) (acc ++ [(partialNum, indices)]) (0, []) xs
      | otherwise = numAcc (i + 1) acc (partialNum, indices) xs

adjToSym :: Int -> Int -> [String] -> Bool
adjToSym row col field = adjToSymH [(0, 1), (0, -1), (1, 0), (-1, 0), (1, 1), (1, -1), (-1, 1), (-1, -1)]
  where
    adjToSymH :: [(Int, Int)] -> Bool
    adjToSymH = any (isSym . \(x, y) -> field !! capY (row + y) !! capX (col + x))
    capX x = max 0 $ min (length (head field) - 1) x
    capY y = max 0 $ min (length field - 1) y
    isSym :: Char -> Bool
    isSym '.' = False
    isSym s
      | isDigit s = False
      | otherwise = True

isNumberValid :: Int -> [String] -> IInt -> Bool
isNumberValid row field (x, is) = any (\col -> adjToSym row col field) is

validNumbers :: [String] -> [Int]
validNumbers field = concatMap (\(row, is) -> map fst $ filter (isNumberValid row field) is) $ zip [0 ..] $ map numbers field

main :: IO ()
main = interact $ show . sum . validNumbers . lines
