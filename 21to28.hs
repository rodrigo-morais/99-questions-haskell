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
rnd_select :: Int -> [a] -> IO [a]
rnd_select _ [] = return []
rnd_select n xs
  | n < 0 = return []
  | otherwise = do pos <- replicateM n $
                    getStdRandom $ randomR (0, (length xs) - 1)
                   return [xs!!p | p <- pos]


-- exercise 24
diff_select :: Int -> Int -> IO [Int]
diff_select n to = diff_select' n [1..to]
 
diff_select' 0 _  = return []
diff_select' _ [] = error "too few elements to choose from"
diff_select' n xs = do r <- randomRIO (0,(length xs)-1)
                       let remaining = take r xs ++ drop (r+1) xs
                       rest <- diff_select' (n-1) remaining
                       return ((xs!!r) : rest)


-- exercise 25
rn_permu :: [a] -> IO [a]
rn_permu xs = rnd_select (length xs) xs