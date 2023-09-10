maximum' :: (Ord a) => [a] -> a
maximum' [x] = x
maximum' (x:xs) = max x (maximum' xs)


fibonacci n = go n (0,1)
    where
        go !n (!a, !b) 
            | n == 0 = a
            | otherwise = go (n - 1) (b, a + b)
        
take' :: (Num i, Ord i) => i -> [a] -> [a]
take' n _ 
    | n <= 0 = []
take' _ [] = []
take' n (x:xs) = x : take' (n - 1) xs

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

repeat' :: a -> [a]
repeat' x = x : repeat' x

zip' :: [a] -> [b] -> [(a, b)]
zip' _ [] = []
zip' [] _ = []
zip' (x : xs) (y : ys) = (x, y) : zip' xs ys

elem' :: (Eq a) => a -> [a] -> Bool
elem' a [] = False
elem' a (x:xs)
    | a == x = True
    | otherwise = a `elem'` xs

quickSort :: (Ord a) => [a] -> [a]
quickSort [] = []
quickSort (x:xs) = 
    let smallerOfEqual = [a | a <- xs, a <= x]
        larger = [a | a <- xs, a > x]
    in quickSort smallerOfEqual ++ [x] ++ quickSort larger

quickSort' :: (Ord a) => [a] -> [a]
quickSort' [] = []
quickSort' (x:xs) =
    let smallerOfEqual = [a | a <- xs, a <= x]
        larger = [a | a <- xs, a > x]
    in quickSort' smallerOfEqual ++ [x] ++ quickSort' larger