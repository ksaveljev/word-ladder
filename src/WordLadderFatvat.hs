import System.Environment (getArgs)
import Data.Char (toLower, isAlpha)
import Data.List ((\\), sortBy, tails, inits)
import Data.Ord (comparing)
import qualified Data.Set as S

type Word = String

type WordSet = S.Set Word

data Node = Node Word [Node]

data DistanceMetric = DistanceMetric (Word -> Word -> Int) (Word -> WordSet)

validChars :: String
validChars = "abcdefghijklmnopqrstuvwxyz"

wordListPath :: String
wordListPath = "/usr/share/dict/words"

createDictionary :: IO WordSet
createDictionary = do
    file <- readFile wordListPath
    return $ S.fromList $ filter (all isAlpha) (map (map toLower) $ words file)

difference :: Word -> Word -> Int
difference x y
  | length x /= length y = 99999
  | otherwise = sum $ zipWith (\c1 c2 -> if c1 == c2 then 0 else 1) x y

transposeChar :: Word -> [Word]
transposeChar [] = []
transposeChar (x:xs) = map (:xs) (validChars \\ [x])

deleteChar :: Word -> [Word]
deleteChar [] = []
deleteChar (_:xs) = [xs]

insertChar :: Word -> [Word]
insertChar [] = []
insertChar word = map (:word) validChars

differenceEdit :: Word -> WordSet
differenceEdit x = edit' x [transposeChar]

editDistanceEdits :: Word -> WordSet
editDistanceEdits x = edit' x [insertChar, transposeChar, deleteChar]

edit' :: Word -> [Word -> [Word]] -> WordSet
edit' w fns = S.fromList $ concat $ zipWith (\x y -> map (\z -> x ++ z) (concatMap (\a -> a y) fns)) (inits w) (tails w)

simple :: DistanceMetric
simple = DistanceMetric difference differenceEdit

edits :: DistanceMetric
edits = DistanceMetric editDistance editDistanceEdits

-- Grabbed from http://www.haskell.org/haskellwiki/Edit_distance
editDistance :: Word -> Word -> Int
editDistance a b =
  last (if lab == 0 then mainDiag
       else if lab > 0 then lowers !! (lab - 1)
                       else{- < 0 -} uppers !! (-1 - lab))
  where
    mainDiag = oneDiag a b (head uppers) (-1 : head lowers)
    uppers = eachDiag a b (mainDiag : uppers) -- upper diagonals
    lowers = eachDiag b a (mainDiag : lowers) -- lower diagonals
    eachDiag _ [] diags = []
    eachDiag a (bch:bs) (lastDiag:diags) = oneDiag a bs nextDiag lastDiag : eachDiag a bs diags
      where nextDiag = head (tail diags)
    oneDiag a b diagAbove diagBelow = thisdiag
      where doDiag [] b nw n w = []
            doDiag a [] nw n w = []
            doDiag (ach:as) (bch:bs) nw n w = me : doDiag as bs me (tail n) (tail w)
              where me = if ach == bch then nw else 1 + min3 (head w) nw (head n)
            firstelt = 1 + head diagBelow
            thisdiag = firstelt : doDiag a b firstelt diagAbove (tail diagBelow)
    lab = length a - length b
    min3 x y z = if x < y then x else min y z

buildGraph :: DistanceMetric -> WordSet -> Word -> Node
buildGraph d@(DistanceMetric dist edits) wordset top = Node top (map (buildGraph d smaller) neighbours)
  where possibleNeighbours = edits top
        neighbours = S.toList (smaller `S.intersection` possibleNeighbours)
        smaller = S.delete top wordset

search :: DistanceMetric -> Node -> Int -> Word -> [Word]
search (DistanceMetric dist _) graph maxDepth goal = search' graph []
  where
    search' (Node end children) path
      | end == goal = end :path
      | length path >= maxDepth = [] -- too deep
      | dist end goal >= maxDepth - length path = [] -- too much difference
      | otherwise = first
        where
          -- Find the best node to search by comparing it against the goal
          costForNextChild :: [(Int, Node)]
          costForNextChild = zip (map (\(Node x _) -> dist x goal) children) children
          bestFirst = map snd $ sortBy (comparing fst) costForNextChild
          -- Best first search
          childRoutes = filter (not . null) $ map (\child -> search' child (end : path)) bestFirst
          first | null childRoutes = []
                | otherwise = head childRoutes

makeLadder :: DistanceMetric -> Int -> Word -> Word -> IO [Word]
makeLadder d maxDepth start end = do
    dict <- createDictionary
    return $ if S.member start dict && S.member end dict
      then search d (buildGraph d dict end) maxDepth start
      else []

main :: IO ()
main = do
    args <- getArgs
    ladder <- makeLadder edits 100 (head args) (args !! 1)
    print ladder
