import Data.Char
import Data.List (groupBy, nub, sort)
import Data.Maybe (isJust, mapMaybe)

type IInt = (Int, [Int]) -- index int
type Gear = (Int, Int) -- x,y position of gear

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

adjOffsets = [(0, 1), (0, -1), (1, 0), (-1, 0), (1, 1), (1, -1), (-1, 1), (-1, -1)]

adjToSym :: Int -> Int -> [String] -> Bool
adjToSym row col field = adjToSymH adjOffsets
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

validNumbers :: [String] -> [[IInt]]
validNumbers field = map (\(row, is) -> filter (isNumberValid row field) is) $ zip [0 ..] $ map numbers field

adjGears :: Int -> IInt -> [String] -> [Gear]
adjGears row num field = nub $ concatMap (\(x, y) -> mapMaybe (\col -> adjGearAtOffs col x y) (snd num)) adjOffsets
  where
    adjGearAtOffs col x y
      | field !! capY (row + y) !! capX (col + x) == '*' = Just (capX (col + x), capY (row + y))
      | otherwise = Nothing
    capX x = max 0 $ min (length (head field) - 1) x
    capY y = max 0 $ min (length field - 1) y

gears :: [String] -> [(Gear, [IInt])]
gears field = map unpack $ groupBy (\a b -> fst a == fst b) $ sort $ cart $ concatMap gearsInRow $ zip [0 ..] $ validNumbers field
  where
    gearsInRow :: (Int, [IInt]) -> [([Gear], IInt)]
    gearsInRow (row, nums) = map (\num -> (adjGears row num field, num)) nums
    cart :: [([Gear], IInt)] -> [(Gear, IInt)]
    cart = concatMap (\(gears, num) -> [(g, num) | g <- gears])
    unpack :: [(Gear, IInt)] -> (Gear, [IInt])
    unpack xs = (fst $ head xs, map snd xs)

gearRatios :: [(Gear, [IInt])] -> [Int]
gearRatios = map (product . map fst . snd) . filter (\(g, nums) -> length nums == 2)

main :: IO ()
main = interact $ show . sum . gearRatios . gears . lines
