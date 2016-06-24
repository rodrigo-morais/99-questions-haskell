import Data.List

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