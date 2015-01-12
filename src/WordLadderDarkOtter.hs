import Prelude hiding (foldl, foldr, foldl1, foldr1, (.), id)
import Data.Foldable (foldl')
import Control.Category (id, (.), (>>>))

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

main :: IO ()
main = undefined
