import Data.List (find)
import Data.Char (toLower)
import Data.Set (Set, member, difference)
import Control.Monad.State (evalState, State, liftM, put, get)
import System.Environment (getArgs)
import System.Exit (exitWith, ExitCode(ExitFailure))
import qualified Data.Set as S

main :: IO ()
main = do
    (start, goal, dict) <- getArgs >>= parse
    fullDictionary <- readDictionary dict
    print $ search start goal (trim fullDictionary start)
  where parse [start, goal, dict] = return (start, goal, dict)
        parse [start, goal] = return (start, goal, "/usr/share/dict/words")
        parse _ = putStrLn "Usage: ladder start goal [ dictionary ]" >> exitWith (ExitFailure 1)
        
        trim :: [String] -> String -> [String]
        trim words start = filter (sameLength start) (lc words)

        sameLength start = (== length start) . length

        lc = map (map toLower)

        print Nothing = putStrLn "No ladder found."
        print (Just a) = mapM_ putStrLn a

readDictionary :: FilePath -> IO [String]
readDictionary path = liftM lines $ readFile path

search start goal words =
    evalState (loop [[start]]) (S.fromList $ filter (/=start) words)
    where
      loop :: [[String]] -> State (Set String) (Maybe [String])
      loop [] = return Nothing
      loop paths = do
        next <- step paths
        let newFringe = fringe next
        words <- get
        put $ words `difference` newFringe
        if goal `member` newFringe
          then return $ Just (winner next)
          else loop next
      winner :: [[String]] -> [String]
      winner paths = case (find ((== goal) . last) paths) of
                       Nothing -> undefined
                       Just a -> a

step :: [[String]] -> State (Set String) [[String]]
step paths = do
    words <- get
    return $ paths >>= augment words

augment :: Set String -> [String] -> [[String]]
augment words path = [ path ++ [n] | n <- ns ]
  where ns = S.elems $ neighbours (last path) words

neighbours :: String -> Set String -> Set String
neighbours word words = S.filter (oneHop word) words
  where oneHop [] [] = False
        oneHop (x:xs) (y:ys) | x /= y = xs == ys
                             | otherwise = oneHop xs ys

fringe :: [[String]] -> Set String
fringe paths = S.fromList (map last paths)
