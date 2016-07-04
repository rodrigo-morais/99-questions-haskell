import System.Random
import Control.Monad (replicateM)
import Data.List (tails, sortBy)
import Data.Tuple.Select


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
lsort :: Ord a => [[a]] -> [[a]]
lsort xs = sortBy (compareLength) xs

compareLength :: (Ord (t a), Foldable t) => t a -> t a -> Ordering
compareLength xs ys
  | length xs < length ys = LT
  | length xs > length ys = GT
  | length xs == length ys = compare xs ys

addSize :: [[a]] -> [([a], Int)]
addSize [] = []
addSize xs = [(head xs, length (head xs))] ++ addSize (tail xs)

addFrequency :: [([a], Int)] -> [([a], Int)] -> [([a], Int, Int)]
addFrequency _ [] = []
addFrequency xs (y:ys) = [((fst y), (snd y), length . filter(\x -> x == (snd y)) $ map (snd) xs)] ++ addFrequency xs ys

compareFrequency :: (Ord a, Ord a1, Ord a2) => (a2, a1, a) -> (a2, a1, a) -> Ordering
compareFrequency (xs, b, c) (ys, d, e)
  | c < e = LT
  | c > e = GT
  | b < d = GT
  | b > d = GT
  | otherwise = compare xs ys

lsort' :: Ord a => [[a]] -> [[a]]
lsort' xs = map (sel1) . sortBy (compareFrequency) $ addFrequency (addSize xs) (addSize xs)