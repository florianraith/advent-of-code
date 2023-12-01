import Data.Char

contains :: String -> String -> Bool
contains x y = crev (reverse x) (reverse y)
  where
    crev :: String -> String -> Bool
    crev [] s = False
    crev [x] s = [x] == s
    crev (x : xs) s = (x : xs) == s || crev xs s

digits :: String -> String
digits [] = []
digits s
  | isDigit $ head s = head s : digits (tail s)
  | contains s "one" = '1' : digits (tail s)    -- there must be a better way, but i'm new to haskell
  | contains s "two" = '2' : digits (tail s)
  | contains s "three" = '3' : digits (tail s)
  | contains s "four" = '4' : digits (tail s)
  | contains s "five" = '5' : digits (tail s)
  | contains s "six" = '6' : digits (tail s)
  | contains s "seven" = '7' : digits (tail s)
  | contains s "eight" = '8' : digits (tail s)
  | contains s "nine" = '9' : digits (tail s)
  | otherwise = digits (tail s)

asNumber :: String -> Int
asNumber s = read $ head s : [last s]

main :: IO ()
main = interact $ show . sum . map (asNumber . digits) . words
