import Data.Char

split :: String -> [String] -> String -> [String]
split tmp acc [] = acc ++ [tmp]
split tmp acc (x : xs)
  | x == ':' || x == ';' = split "" (acc ++ [tmp]) xs
  | x == ' ' = split tmp acc xs
  | otherwise = split (tmp ++ [x]) acc xs

endsWith :: String -> String -> Bool
endsWith s [] = False
endsWith s [x] = [x] == s
endsWith s (x : xs) = (x : xs) == s || endsWith s xs

splitComma :: String -> [String]
splitComma s = case dropWhile (== ',') s of
  "" -> []
  s' -> w : splitComma s''
    where
      (w, s'') = break (== ',') s'

splitDraws :: [String] -> [[String]]
splitDraws = map splitComma . tail

sumColorParts :: [[String]] -> [(Int, Int, Int)]
sumColorParts =
  map
    ( \x ->
        ( sum . map (toInt 3) $ filter (endsWith "red") x
        , sum . map (toInt 5) $ filter (endsWith "green") x
        , sum . map (toInt 4) $ filter (endsWith "blue") x
        )
    )
  where
    toInt offset = read . reverse . drop offset . reverse

isValid :: [(Int, Int, Int)] -> Bool
isValid = all (\(r, g, b) -> r <= 12 && g <= 13 && b <= 14)

main :: IO ()
main = interact $ show . sum . map fst . filter snd . zip [1 ..] . map (isValid . sumColorParts . splitDraws . split "" []) . lines
