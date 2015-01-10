{-# LANGUAGE BangPatterns #-}

import Prelude hiding (foldl, foldr, foldl1, foldr1, (.), id)

data Trie a = Trie !Bool [Node a] deriving (Show, Eq)
data Node a = Node ! a   (Trie a) deriving (Show, Eq)

main :: IO ()
main = undefined
