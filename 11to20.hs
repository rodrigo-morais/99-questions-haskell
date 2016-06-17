import Data.List (group)

-- exercise 11
encodeModified :: (Eq a) => [a] -> [(String, Int, a)]
encodeModified [] = []
encodeModified (x:xs) = 
  let
    qty =
      length $ x : takeWhile (== x) xs

    modifier =
      case qty of
        1 -> "Single"
        _ -> "Multiple"
  in
    [(modifier, qty, x)] ++ encodeModified (dropWhile (==x) xs)

data ListItem a = Single a | Multiple Int a
  deriving (Show)

encode :: (Eq a) => [a] -> [(Int, a)]
encode [] = []
encode (x:xs) = 
    [(length $ x : takeWhile (== x) xs, x)] ++ encode (dropWhile (==x) xs)

encodeModified' :: (Eq a) => [a] -> [ListItem a]
encodeModified' = map encodeHelper . encode
  where
    encodeHelper (1, x) = Single x
    encodeHelper (n, x) = Multiple n x


-- exercise 12
decodeModified :: (Eq a) => [ListItem a] -> [a]
decodeModified = concatMap decodeHelper
  where
    decodeHelper (Single x) = [x]
    decodeHelper (Multiple n x) = replicate n x


-- exercise 13
encode' :: Eq a => [a] -> [(Int,a)]
encode' = foldr helper []
    where
      helper x [] = [(1,x)]
      helper x (y@(a,b):ys)
        | x == b    = (1+a,x):ys
        | otherwise = (1,x):y:ys

encodeDirect :: Eq a => [a] -> [ListItem a]
encodeDirect = map encodeHelper . encode'
    where
      encodeHelper (1,x) = Single x
      encodeHelper (n,x) = Multiple n x


-- exercise 14
dupli :: [a] -> [a]
dupli xs = foldr (++) [] $ map (replicate 2) xs


-- exercise 15
repli :: [a] -> Int -> [a]
repli xs n = foldr (++) [] $ map (replicate n) xs

-- exercise 16
dropEvery :: [a] -> Int -> [a]
dropEvery [] _ = []
dropEvery xs n = (take (n-1) xs) ++ dropEvery (drop n xs) n

-- exercise 17
split :: [a] -> Int -> ([a],[a])
split [] _ = ([], [])
split xs n = splitAt n xs

split' :: [a] -> Int -> ([a],[a])
split' [] _ = ([], [])
split' xs n = 
  let
    takeLeft =
      take n xs

    takeRight =
      drop n xs
  in
    (takeLeft, takeRight)

-- exercise 18
slice :: [a] -> Int -> Int -> [a]
slice [] _ _ = []
slice xs begin end = take (end - (begin - 1)) $ drop (begin - 1) xs 