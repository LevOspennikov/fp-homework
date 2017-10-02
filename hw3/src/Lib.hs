module Lib
    ( WeekDay,
      dayNum,
      nextDay,
      afterDays,
      isWeekend,
      daysToParty,
      vecTo3D,
      vecAdd,
      vecCrossProd,
      vecDist,
      vecLen,
      vecDotProd,
      addTwoNats,
      mulTwoNats,
      minusTwoNats,
      natToInt,
      intToNat,
      Nat,
      empty,
      sizeTree,
      contains,
      insert,
      fromList

    ) where

import           Data.Char (isDigit)

data WeekDay = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday deriving (Enum, Show)

dayNum :: WeekDay -> Int
dayNum = fromEnum

nextDay :: WeekDay -> WeekDay
nextDay weekDay
     | dayNum weekDay == 6 = Monday
     | otherwise = toEnum (dayNum weekDay + 1) :: WeekDay

afterDays :: WeekDay -> Int -> WeekDay
afterDays weekDay days
    | days < 0 = error "Bad days number"
    | days >= 7 = afterDays weekDay (days `mod` 7)
    | days == 0 = weekDay
    | otherwise = afterDays (nextDay weekDay) (days - 1)

isWeekend :: WeekDay -> Bool
isWeekend weekDay = dayNum weekDay >= 5

daysToParty :: WeekDay -> Int
daysToParty weekDay
    | dayNum weekDay == 4 = 0
    | otherwise = 1 + daysToParty (nextDay weekDay)



data Vector a = Vector2D a a | Vector3D a a a

packVector :: Vector a -> [a]
packVector (Vector2D x y)   = [x, y]
packVector (Vector3D x y z) = [x, y, z]

vecTo3D :: (Num a) => Vector a -> Vector a
vecTo3D (Vector2D x y) = Vector3D x y 0
vecTo3D v              = v

vecAdd :: Vector Double -> Vector Double -> Vector Double
vecAdd (Vector2D x1 y1) (Vector2D x2 y2) = Vector2D (x1 + x2) (y1 + y2)
vecAdd a b = addVec (vecTo3D a) (vecTo3D b)
    where addVec (Vector3D x1 y1 z1) (Vector3D x2 y2 z2) = Vector3D (x1 + x2) (y1 + y2) (z1 + z2)

vecCrossProd :: Vector Double -> Vector Double -> Double
vecCrossProd (Vector2D x1 y1) (Vector2D x2 y2) = x1 * x2 + y1 * y2
vecCrossProd a b = vecCrProd (vecTo3D a) (vecTo3D b)
    where
        vecCrProd (Vector3D x1 y1 z1) (Vector3D x2 y2 z2) = x1 * x2 + y1 * y2 + z1 * z2

vecReverse :: Vector Double -> Vector Double
vecReverse (Vector2D x y)   = Vector2D (-x) (-y)
vecReverse (Vector3D x y z) = Vector3D (-x) (-y) (-z)

vecDist :: Vector Double -> Vector Double -> Double
vecDist a b = vecLen $ vecAdd a $ vecReverse b

vecLen :: Vector Double -> Double
vecLen = sqrt . sum . map (^ 2) . packVector

vecDotProd :: Vector Double -> Vector Double -> Vector Double
vecDotProd a b = Vector3D (y1 * z2 - y2 * z1) (z1 * x2 - z2 * x1) (x1 * y2 - x2 * y1)
    where
        (Vector3D x1 y1 z1) = vecTo3D a
        (Vector3D x2 y2 z2) = vecTo3D b


data Nat = Z | S Nat

addTwoNats :: Nat -> Nat -> Nat
addTwoNats Z Z          = Z
addTwoNats Z (S xs)     = S xs
addTwoNats (S xs) Z     = S xs
addTwoNats (S xs) (S y) = S (addTwoNats xs (S y))

mulTwoNats :: Nat -> Nat -> Nat
mulTwoNats Z _          = Z
mulTwoNats _ Z          = Z
mulTwoNats (S xs) (S y) = addTwoNats (mulTwoNats xs (S y)) (S y)

minusTwoNats :: Nat -> Nat -> Nat
minusTwoNats Z Z          = Z
minusTwoNats Z (S _)      = error "First lower than second"
minusTwoNats (S y) Z      = S y
minusTwoNats (S xs) (S y) = minusTwoNats xs y

natToInt :: Nat -> Integer
natToInt Z      = 0
natToInt (S xs) = 1 + natToInt xs

intToNat :: Integer -> Nat
intToNat 0 = Z
intToNat x
        | x > 0 = S (intToNat (x - 1))
        | otherwise = error "Negative number"

eqNuts :: Nat -> Nat -> Bool
eqNuts Z Z          = True
eqNuts Z (S _)      = False
eqNuts (S _) Z      = False
eqNuts (S xs) (S y) = eqNuts xs y

gtNuts :: Nat -> Nat -> Bool
gtNuts Z Z          = True
gtNuts Z (S _)      = False
gtNuts (S _) Z      = True
gtNuts (S xs) (S y) = eqNuts xs y

instance Eq Nat where
    x == y = eqNuts x y

instance Ord Nat where
    x > y = gtNuts x y
    x <= y = (||) (gtNuts y x)  (eqNuts x y)

instance Num Nat where
    x + y = addTwoNats x y
    x * y = mulTwoNats x y
    x - y = minusTwoNats x y
    fromInteger = intToNat

data Tree a = Leaf | Node a (Tree a) (Tree a)
                deriving(Show)

empty :: (Ord a) => Tree a -> Bool
empty Leaf = True
empty  _   = False

sizeTree :: Tree a -> Integer
sizeTree Leaf           = 0
sizeTree (Node _ t1 t2) = 1 + sizeTree t1 + sizeTree t2

contains :: (Ord a) => Tree a -> a -> Bool
contains Leaf _ = False
contains (Node v t1 t2) x
    | x == v = True
    | x  < v = contains t1 x
    | x  > v = contains t2 x

insert :: (Ord a) => Tree a -> a -> Tree a
insert Leaf x = Node x Leaf Leaf
insert (Node v t1 t2) x
    | v == x = Node v t1 t2
    | v > x = Node v t1 (insert t2 x)
    | v < x = Node v (insert t1 x) t2

fromList ::  (Ord a) => [a] -> Tree a
fromList []     = Leaf
fromList (xs:x) = insert (fromList x) xs

