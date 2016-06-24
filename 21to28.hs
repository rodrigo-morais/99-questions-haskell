import System.Random
import Control.Monad (replicateM)


-- exercise 21
insertAt :: a -> [a] -> Int -> [a]
insertAt newValue xs pos =
  case xs of
    [] -> []
    _ -> take (pos - 1) xs ++ [newValue] ++ drop (pos - 1) xs


insertAt' :: a -> [a] -> Int -> [a]
insertAt' newValue xs pos =
  let
    (ys,zs) =
      splitAt (pos - 1) xs

  in
    ys ++ newValue:zs


-- exercise 22
range :: Int -> Int -> [Int]
range begin end
  | begin > end = []
  | otherwise = range begin (end - 1) ++ [end]


range' :: Int -> Int -> [Int]
range' begin end
  | begin > end = []
  | otherwise = begin : range (begin + 1) end


range'' :: Int -> Int -> [Int]
range'' begin end = [begin..end]


range''' :: Int -> Int -> [Int]
range''' begin end = take (end - begin + 1) $ iterate (+1) begin


-- exercise 23
rnd_select :: [a] -> Int -> IO [a]
rnd_select [] _ = return []
rnd_select xs n
  | n < 0 = return []
  | otherwise = do pos <- replicateM n $
                    getStdRandom $ randomR (0, (length xs) - 1)
                   return [xs!!p | p <- pos]