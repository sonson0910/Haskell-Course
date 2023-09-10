
module Buoi5.HamBacCao where

compareWithHundred' :: Int -> Ordering
compareWithHundred' x = compare 100 x

divideByTen :: (Floating a) => a -> a
divideByTen = (/10)


applyTwice' :: (a -> a) -> a -> a
applyTwice' f x = f (f x)


zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f = g
    where g x y = f y x

flip'' :: (a -> b -> c) -> b -> a -> c
flip'' f y x = f x y

map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs

filter' :: (a->Bool) -> [a] -> [a]
filter' _ [] = []
filter' p (x:xs)
    | p x = x:filter' p xs
    | otherwise = filter' p xs

quickSort' :: (Ord a) => [a] -> [a]
quickSort' [] = []
quickSort' (x:xs) =
    let smallerOrEqual = filter' (<=x) xs
        larger = filter' (>x) xs
    in quickSort' smallerOrEqual ++ [x] ++ quickSort' larger

largestDivisible :: Integer
largestDivisible = head (filter' p [100000,99999..])
    where p x = x `mod` 3829 == 0

chain :: Integer -> [Integer]
chain 1 = [1]
chain n
    | even n = n:chain (n `div` 2)
    | odd n = n:chain (n * 3 + 1)


numLongChains :: Int
numLongChains = length (filter isLong (map chain [1..100]))
    where isLong xs = length xs > 15

numLongChains' :: Int
numLongChains' = length (filter' (\xs -> length xs > 15) (map chain [1..100]))

flip''' :: (a -> b -> c) -> b -> a -> c
flip''' f = \x y -> f y x

sum' :: (Num a) => [a] -> a
sum' xs = foldl (\acc x -> acc + x) 0 xs

sum'' :: (Num a) => [a] -> a
sum'' = foldl (+) 0

map'' :: (a -> b) -> [a] -> [b]
map'' f xs = foldr (\x acc -> f x : acc) [] xs