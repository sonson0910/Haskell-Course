-- type String = [Char]
import qualified Data.Map as Map
phoneBook :: [(String, String)]
phoneBook =
    [("betty", "555-2938")
    ,("bonnie", "452-2928")
    ,("patsy", "493-2928")
    ,("lucille", "205-2928")
    ,("wendy", "939-8282")
    ,("penny", "853-2492")
    ]


-- type PhoneBook = [(String, String)]
type PhoneNumber = String
type Name = String
type PhoneBook = [(Name, PhoneNumber)]

inPhoneBook :: Name -> PhoneNumber -> PhoneBook -> Bool
inPhoneBook name pnumber pbook = (name, pnumber) `elem` pbook

type AssocList k v = [(k, v)]

type IntMap v = Map.Map Int v

-- data Either a b = Left a | Right b deriving (Eq, Ord, Show, Read)

data LockerState = Taken | Free deriving (Show, Eq)

type Code = String

type LockerMap = Map.Map Int (LockerState, Code)


lockerLookup :: Int -> LockerMap -> Either String Code
lockerLookup lockerNumber map = case Map.lookup lockerNumber map of Nothing -> Left $ "Locker " ++ show lockerNumber ++ " doesn't exist!" 
                                                                    Just (state, code) -> if state /= Taken
                                                                                            then Right code
                                                                                            else Left $ "Locker " ++ show lockerNumber
                                                                                            ++ " is already taken!"

lockers :: LockerMap
lockers = Map.fromList
    [(100,(Taken, "ZD39I"))
    ,(101,(Free, "JAH3I"))
    ,(103,(Free, "IQSA9"))
    ,(105,(Free, "QOTSA"))
    ,(109,(Taken, "893JJ"))
    ,(110,(Taken, "99292"))
    ]

-- data List a = Empty | Cons a (List a) deriving (Show, Read, Eq, Ord)

-- data List a = Empty | Cons {
--     listHead :: a, 
--     listTail :: List a
-- } deriving (Show, Read, Eq, Ord)

infixr 5 :-:
data List a = Empty | a :-: (List a) deriving (Show, Read, Eq, Ord)

-- infixr 5 ^++
-- (^++) :: [a] -> [a] -> [a]
-- []     ^++ ys = ys
-- (x:xs) ^++ ys = x : (xs ^++ ys)

infixr 5 ^++
(^++) :: List a -> List a -> List a
Empty  ^++ ys = ys
(x :-: xs) ^++ ys = x :-: (xs ^++ ys)



data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show)


singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = singleton x 
treeInsert x (Node a left right) 
    | x == a = Node x left right
    | x < a = Node a (treeInsert x left) right
    | x > a = Node a left (treeInsert x right)

treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem x EmptyTree = False
treeElem x (Node a left right)
    | x == a = True
    | x < a = treeElem x left
    | x > a = treeElem x right

