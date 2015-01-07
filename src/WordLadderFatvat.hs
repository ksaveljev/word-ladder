import System.Environment (getArgs)
import Data.Char (toLower, isAlpha)
import Data.List (minimumBy)
import Data.Ord (comparing)
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

search :: Node -> Int -> String -> [String]
search graph maxDepth goal = search' graph maxDepth goal []

search' :: Node -> Int -> String -> [String] -> [String]
search' (Node end children) maxDepth goal path
  | end == goal = end : path
  | null children = []
  | length path >= maxDepth = [] -- too deep
  | difference end goal >= maxDepth - length path = [] -- too much difference
  | otherwise = first
    where
      childRoutes = filter (not . null) $ map (\child -> search' child maxDepth goal (end : path)) children
      first | null childRoutes = []
            | otherwise = head childRoutes
      quickest | null childRoutes = []
               | otherwise = minimumBy (comparing length) childRoutes

makeLadder :: Int -> String -> String -> IO [String]
makeLadder maxDepth start end
  | length start /= length end = error "Only two string of equal length are currently supported."
  | otherwise = do
      dict <- createDictionary (length start)
      return $
        if S.member start dict && S.member end dict
          then search (buildGraph dict start) maxDepth end
          else []

main :: IO ()
main = do
    args <- getArgs
    ladder <- makeLadder 100 (head args) (args !! 1)
    print ladder
