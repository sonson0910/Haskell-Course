doubleMe :: Num a => a -> a
doubleMe x = x + x

doubleUs x y = x*2 + y*2

doubleSmallNumber :: (Ord a, Num a) => a -> a
doubleSmallNumber x = if x > 100
    then x
    else x * 2

doubleSmallNumber' :: (Num a, Ord a) => a -> a
doubleSmallNumber' x = (if x > 100 then x else x * 2) + 1

boomBangs :: Integral a => [a] -> [String]
boomBangs xs = [if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x]

length' xs = sum [1 | _ <- xs]

removeNonUppercase :: String -> String
removeNonUppercase st = [c | c <- st, c `elem` ['A'..'Z']]

addThree :: Int -> Int -> Int -> Int
addThree x y z = x + y + z

factorial :: Integer -> Integer
factorial n = product [1..n]

cicumference :: Float -> Float
cicumference r = 2 * pi * r

cicumference' :: Double -> Double
cicumference' r = 2 * pi * r
