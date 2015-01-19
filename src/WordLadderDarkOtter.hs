import Prelude hiding (foldl, foldr, foldl1, foldr1, (.), id)
import Data.Foldable (foldl', foldr, forM_)
import Data.Monoid (mempty)
import Control.Category (id, (.), (>>>))
import Control.Applicative ((<$>))
import System.Environment (getArgs)

data Trie a = Trie !Bool [Node a] deriving (Show, Eq)
data Node a = Node !a    (Trie a) deriving (Show, Eq)

buildTrie :: (Eq a) => [[a]] -> Trie a
buildTrie [] = Trie False [] -- The empty tree
buildTrie xs = Trie hasEmpty steps
  where
    -- Remove any empty items from the list
    (hasEmpty, toAdd) = preFilter xs id

    preFilter     []  c = (False, c [])
    preFilter ([]:ys) c = (True, c $ filter (not . null) ys)
    preFilter ( y:ys) c = preFilter ys $ (y:) >>> c

    steps = makeSteps toAdd

    makeSteps [] = []
    makeSteps (y:ys) = Node first next : makeSteps different
      where
        first = head y
        (same, different) = span ((first ==) . head) ys
        next = buildTrie $ fmap tail $ y : same

foldTrie :: (Eq a) => ([a] -> b -> b) -> b -> Trie a -> b
foldTrie f = foldTrie' id
  where
    foldTrie' before e (Trie yield children) = if yield then f (before []) m else m
      where
        m = foldDigits' before e children

    foldDigits' _ e [] = e
    foldDigits' before e (x:xs) = foldDigit' before (foldDigits' before e xs) x

    foldDigit' before e (Node c trie) = foldTrie' (before . (c:)) e trie

findExact :: (Eq a) => [a] -> Trie a -> [[a]]
findExact k t = replicate (count k 0 t) k
  where
    count []     n (Trie yield        _) = if yield then n + 1 else n
    count (x:xs) n (Trie _     children) = foldl' (folder x xs) n children

    folder x xs n (Node c trie) = if x == c then count xs n trie else n

findFuzzy :: (Eq a, Integral n) => n -> [a] -> Trie a -> [[a]]
findFuzzy = findFuzzy' id []
  where
    findFuzzy' b e 0 k t = (fmap b $ findExact k t) ++ e
    findFuzzy' b e _ [] (Trie yield _) = if yield then b [] :e else e
    findFuzzy' b e n (x:xs) (Trie _ children) = foldr (folder b x xs n) e children

    folder b x xs n (Node c trie) e = n' `seq` findFuzzy' (b . (c:)) e n' xs trie
      where n' = if x == c then n else n - 1

distance :: (Eq a, Integral n) => [a] -> [a] -> n
distance l r = distance' l r 0
  where
    distance' [] [] n = n
    distance' (l:ls) (r:rs) n = distance' ls rs $! n'
      where n' = if l == r then n else n + 1

ladders :: (Ord a, Integral n) => Trie a -> [a] -> [a] -> [(n, [[a]])]
ladders trie from target = consume $ push [(distance from target, 0, [from])] mempty
  where
    consume queue = maybe [] produce $ pop queue
    produce ((_, cost, p@(mid:_)), queue) = if mid == target
                                              then (cost, p) : consume queue
                                              else consume nextQueue
      where
        nextQueue = push (stepFunc <$> findFuzzy 1 mid trie) queue

        stepFunc step = c `seq` e `seq` (e, c, step:p)
          where
            c = cost + 1
            e = c + distance step target

main :: IO ()
main = do
    (from:to:_) <- getArgs
    let answers = ladders wordTrie from to
    let best = takeWhile ((==) (fst $ head $ answers) . fst) answers
    forM_ fmap (reverse . snd) best $ \answer -> do
      putStrLn "---------"
      forM_ answer $ show >>> putStrLn
