import Data.Char

main :: IO ()
main = interact $ show . sum . map (asNumber . filter isDigit) . words
  where asNumber s = read $ head s : [last s]
