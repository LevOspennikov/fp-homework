{-# LANGUAGE InstanceSigs  #-}
{-# LANGUAGE TypeOperators #-}


module Lib
       ( eval,
        Expr(..),
        ArithmeticError(..)
       ) where


import           Control.Applicative
import           Control.Category
import           Control.Monad
import           Data.Maybe

data Expr = Minus Expr Expr | Plus Expr Expr | Pow Expr Expr | Mul Expr Expr | Div Expr Expr | Constant Int
            deriving (Show)

data ArithmeticError = DivisionByZero | ExponentiationInNegative
            deriving (Show, Eq)
zeroLess :: Either ArithmeticError Int -> Bool
zeroLess (Left x)  = False
zeroLess (Right x) = x <= 0


eval :: Expr -> Either ArithmeticError Int
eval (Minus x y)  = liftA2 (-) (eval x) (eval y)
eval (Plus x y)   = liftA2 (+) (eval x) (eval y)
eval (Mul x y)    = liftA2 (*) (eval x) (eval y)
eval (Pow x y)    = if zeroLess (eval y) then Left ExponentiationInNegative else liftA2 (^) (eval x) (eval y)
eval (Constant x) = Right x
eval (Div x y)    = if eval y == Right 0 then Left DivisionByZero else liftA2 (div) (eval x) (eval y)



data a ~> b
    = Partial   (a -> Maybe b) -- a partial function
    | Defaulted (a ~> b) b     -- a partial function with a default value

partial     :: (a -> Maybe b) -> a ~> b
partial = Partial

total       :: (a -> b) -> a ~> b
total f 	= Partial (\x -> Just (f x))

apply       :: (a ~> b) -> a -> Maybe b
apply (Partial f) t = f t
apply (Defaulted f def) val = if isNothing (apply f val) then Just def else apply f val

applyOrElse :: (a ~> b) -> a -> b -> b
applyOrElse f a def = if isNothing (apply f a) then def else t
                            where
                              (Just t) = apply f a

withDefault :: (a ~> b) -> b -> (a ~> b)  -- Add a default value to a partial function. If the function was already
                                          -- defaulted, override the value with the new default.
withDefault t@(Partial f)   = Defaulted t
withDefault (Defaulted f b) = Defaulted f

isDefinedAt :: (a ~> b) -> a -> Bool
isDefinedAt f a = isNothing (apply f a)

orElse      :: (a ~> b) -> (a ~> b) -> a ~> b  -- Create a new partial function where the domain is the combination
                                               -- of both partial functions. Priority is given to the first partial function
                                               -- in case of conflict.
orElse f g = Partial (\x ->  if isNothing (apply f x) then apply g x else apply f x)

instance Category (~>) where
 id :: a ~> a
 id = Partial (\x -> Just x)
(.) :: b ~> c -> a ~> b -> a ~> c
(.) first second = Partial (\x -> (apply second x) >>= (apply first))

-- identity:      id . f ≡ f . id ≡ f
-- Partal (\x -> Just x) . f ≡ f . Partal (\x -> Just x) ≡ f
