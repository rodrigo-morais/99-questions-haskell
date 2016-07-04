import System.Random
import Control.Monad (replicateM)
import Data.List (tails, sortBy)


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


-- exercise 26
combinations :: Int -> [a] -> [[a]]
combinations 0 _  = [ [] ]
combinations n xs = [ y:ys | y:xs' <- tails xs
                           , ys <- combinations (n-1) xs']

combinations' :: Int -> [a] -> [[a]]
combinations' 0 _      = [[]]
combinations' _ []     = []
combinations' m (x:xs) = map (x:) (combinations (m-1) xs) ++ combinations m xs


-- exercise 27
group :: [Int] -> [a] -> [[a]]
group n xs =
  case xs of
    [] -> []
    _ -> [take (head n) xs] ++ group (tail n) (drop (head n) xs)

group' :: [Int] -> [a] -> [[a]]
group' n xs =
  case n of
    [] -> []
    _ -> combinations (head n) xs ++ group' (tail n) xs


combination :: Int -> [a] -> [([a],[a])]
combination 0 xs     = [([],xs)]
combination n []     = []
combination n (x:xs) = ts ++ ds
  where
    ts = [ (x:ys,zs) | (ys,zs) <- combination (n-1) xs ]
    ds = [ (ys,x:zs) | (ys,zs) <- combination  n    xs ]

group'' :: [Int] -> [a] -> [[[a]]]
group'' [] _ = [[]]
group'' (n:ns) xs =
  [ g:gs | (g, rs) <- combination n xs
         , gs <- group'' ns rs ]


-- exercise 28
lsort :: [[a]] -> [[a]]
lsort xs = map (fst) $ sortBy (compareTuples) (addSize xs)

addSize :: [[a]] -> [([a], Int)]
addSize [] = []
addSize xs = [(head xs, length (head xs))] ++ addSize (tail xs)

compareTuples :: Ord b => (x, b) -> (x1, b) -> Ordering
compareTuples (xs, b) (ys, c)
  | b < c = LT
  | b > c = GT
  | b == c = compare b c