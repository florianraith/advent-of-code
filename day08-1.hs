import Data.Map (Map, fromList, (!))

type Mappings = Map String (String, String)

parseMapping :: String -> (String, (String, String))
parseMapping = (\[start, _, left, right] -> (start, (init $ tail left, init right))) . words

parse :: [String] -> (String, Mappings)
parse (dirs : _ : mappings) = (cycle dirs, fromList $ map parseMapping mappings)

takeDir :: Char -> String -> Mappings -> String
takeDir 'L' loc mappings = fst $ mappings ! loc
takeDir 'R' loc mappings = snd $ mappings ! loc

calcSteps :: String -> Int -> (String, Mappings) -> Int
calcSteps "ZZZ" steps _ = steps
calcSteps loc steps (dirs, mappings) = calcSteps (takeDir (head dirs) loc mappings) (steps + 1) (tail dirs, mappings)

main :: IO ()
main = interact $ show . calcSteps "AAA" 0 . parse . lines
