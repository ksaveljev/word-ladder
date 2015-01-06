import Data.Char (toLower, isAlpha)
import qualified Data.Set as S

type WordSet = S.Set String

data Node = Node String [Node] deriving Show

wordListPath :: String
wordListPath = "/usr/share/dict/words"

createDictionary :: Int -> IO WordSet
createDictionary n = do
    file <- readFile wordListPath
    return $ S.fromList $ filter (\x -> length x == n && all isAlpha x) (map (map toLower) $ words file)

neighbour :: String -> String -> Bool
neighbour x y = difference x y == 1

difference :: String -> String -> Int
difference [] [] = 0
difference (x:xs) (y:ys) | x == y = difference xs ys
                         | otherwise = 1 + difference xs ys
difference _ _ = error "Two strings must be the same length"

buildGraph :: WordSet -> String -> Node
buildGraph wordset top = Node top (map (buildGraph smaller) neighbours)
  where neighbours = S.toList (S.filter (neighbour top) smaller)
        smaller = S.delete top wordset

main :: IO ()
main = undefined
