data a ~> b
    = Partial   (a -> Maybe b) -- a partial function
    | Defaulted (a ~> b) b     -- a partial function with a default value

partial     :: (a -> Maybe b) -> a ~> b
partial = Partial

total       :: (a -> b) -> a ~> b
total f 	= Partial (\x -> Just (f x))

apply       :: (a ~> b) -> a -> Maybe b
apply (Partial f) t = f t
apply (Defaulted f def) val = if isNothing (apply f val) then def else apply f val

applyOrElse :: (a ~> b) -> a -> b -> b
applyOrElse f a def = if isNothing (apply f val) then def else apply f val

withDefault :: (a ~> b) -> b -> (a ~> b)  -- Add a default value to a partial function. If the function was already
                                          -- defaulted, override the value with the new default.
withDefault t@(Partial f)   = Defaulted t
withDefault (Defaulted f b) = Defaulted f

isDefinedAt :: (a ~> b) -> a -> Bool
isDefinedAt f a = isNothing (f a)

orElse      :: (a ~> b) -> (a ~> b) -> a ~> b  -- Create a new partial function where the domain is the combination
                                               -- of both partial functions. Priority is given to the first partial function
                                               -- in case of conflict.
orElse f g = \x -> if isNothing (apply f x) then apply g x else apply f x
