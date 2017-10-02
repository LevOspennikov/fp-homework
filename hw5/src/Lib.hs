module Lib
    ( maybeConcat
    ) where


maybeConcat :: (Monoid t) => [Maybe t] -> t
maybeConcat []            = mempty
maybeConcat ((Nothing):x) = maybeConcat x
maybeConcat ((Just t):x)  = mappend t (maybeConcat x)
