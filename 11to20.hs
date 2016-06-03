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
