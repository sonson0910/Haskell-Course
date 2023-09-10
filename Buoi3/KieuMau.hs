lucky :: Int -> String
lucky 7 = "LYCKY NUMBER SEVEN!"
lucky x = "Sorry, you're out of luck, pal!"

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)

addVectors :: (Double, Double) -> (Double, Double) -> (Double, Double)
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

first :: (a,b,c) -> a
first (x,_,_) = x

seccond :: (a,b,c) -> b
seccond (_,b,_) = b

third :: (a,b,c) -> c
third (_,_,c) = c   

head' :: [a] -> a
head' [] = error "Can't call head on an empty list, dummy!"
head' (x:_) = x


firstLetter :: String -> String
firstLetter "" = "Empty string, whoops!"
firstLetter all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x]

bmiTell :: Double -> String
bmiTell bmi
    | bmi <= 18.5 = "You're underweight, you emo, you!"
    | bmi <= 25 = "You're supposedly normal. Pffft, I bet your ugly!"
    | bmi <= 30 = "You're fat! Loss some weight, fatty!"
    | otherwise = "You're a whale, congratulations!"

bmiTell' :: Double -> Double -> String
bmiTell' weight height
    | bmi <= 18.5 = "You're underweight, you emo, you!"
    | bmi <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly!"
    | bmi <= 30.0 = "You're fat! Lose some weight, fatty!"
    | otherwise = "You're a whale, congratulations!"
    where bmi = weight / height ^ 2

max' :: (Ord a) => a -> a -> a
max' a b
    | a <= b = b
    | otherwise = a

myCompare :: (Ord a) => a -> a -> Ordering
a `myCompare` b
    | a == b = EQ
    | a <= b = LT
    | otherwise = GT

bmiTell'' :: Double -> Double -> String
bmiTell'' weight height
    | bmi <= skinny = "You're underweight, you emo, you!"
    | bmi <= normal = "You're supposedly normal. Pffft, I bet you're ugly!"
    | bmi <= fat = "You're fat! Lose some weight, fatty!"
    | otherwise = "You're a whale, congratulations!"
    where bmi = weight / height ^ 2
        --   skinny = 18.5
        --   normal = 25.0
        --   fat = 30.0
          (skinny, normal, fat) = (18.5, 25.0, 30.0)


badGreeting :: String
badGreeting = "Oh! Pfft. It's you."

niceGreeting :: String
niceGreeting = "Hello! So very nice to see you,"

greet :: String -> String
greet "Juan" = niceGreeting ++ " Juan!"
greet "Fernado" = niceGreeting ++ " Fernado!"
greet name = badGreeting ++ " " ++ name

initials :: String -> String -> String
initials firstname lastname = [f] ++ ". " ++ [l] ++ ". "
    where (f:_) = firstname
          (l:_) = lastname

calcBmis :: [(Double, Double)] -> [Double]
calcBmis xs = [bmi w h | (w, h) <- xs]
    where bmi weight height = weight / height ^ 2

cylinder :: Double -> Double -> Double
cylinder r h = 
    let sideArea = 2 * pi * r * h
        topArea = pi *  r ^ 2
    in sideArea + 2 * topArea

calcBmis' :: [(Double, Double)] -> [Double]
calcBmis' xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2]

calcBmis'' :: [(Double, Double)] -> [Double]
calcBmis'' xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2, bmi > 25.0]

head'' :: [a] -> a
head'' [] = error "No head for empty lists!" 
head'' (x:_) = x

head''' :: [a] -> a
head''' xs = case xs of [] -> error "No head for empty lists!"
                        (x:_) -> x

describeList :: [a] -> String
describeList ls = "The list is " ++ case ls of [] -> "empty."
                                               [x] -> "a singleton list."
                                               xs -> "a longer list"

