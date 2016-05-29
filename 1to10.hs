import Data.List (group)

-- Exercise 1
myLast :: [a] -> a
myLast lst = last lst

-- Exercise 2
myButLast :: [a] -> a
myButLast lst = 
  case (length lst) of
    0 -> error "No end for empty lists!"
    1 -> error "No end for empty lists!"
    2 -> head lst
    _ -> myButLast (tail lst)

myButLast' :: [a] -> a
myButLast' lst = 
  case lst of
    [x,_] -> x
    (_:xs) -> myButLast' xs

myButLast'' :: [a] -> a
myButLast'' = last . init

-- Exercise 3
elementAt :: [a] -> Int -> a
elementAt lst pos = lst !! (pos - 1)

-- Exercise 4
myLength :: [a] -> Int
myLength lst = length lst

myLength' :: [a] -> Int
myLength' lst =
  case lst of
    [] -> 0
    _ -> 1 + (myLength $ tail lst)

myLength'' :: [a] -> Int
myLength'' = sum . map (\_->1)

-- Exercise 5
myReverse :: [a] -> [a]
myReverse = reverse

myReverse' :: [a] -> [a]
myReverse' lst =
  case lst of
    [] -> []
    (x:xs) -> myReverse xs ++ [x]

myReverse'' :: [a] -> [a]
myReverse'' xs = foldr (\x fId empty -> fId (x : empty)) id xs []

-- Exercise 6
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome lst = lst == (reverse lst)

-- Exercise 7
data NestedList a = Elem a | List [NestedList a]

flatten :: NestedList a -> [a]
flatten lst =
  case lst of
    Elem a -> [a]
    List (x:xs) -> flatten x ++ flatten (List xs)
    List [] -> []

-- Exercise 8
compress :: (Eq a) => [a] -> [a]
compress = map head . group

-- Exercise 9
pack :: (Eq a) => [a] -> [[a]]
pack [] = []
pack (x:xs) = (x : takeWhile (== x) xs) : pack (dropWhile (==x) xs)

-- Exercise 10
encode :: (Eq a) => [a] -> [(Int, a)]
encode [] = []
encode (x:xs) = [(length $ x : takeWhile (== x) xs, x)] ++ encode (dropWhile (==x) xs)

encode' :: (Eq a) => [a] -> [(Int, a)]
encode' = map (\x -> (length x, head x)) . group