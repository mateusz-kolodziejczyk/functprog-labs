module Util where


-- Take in a foldable and filter it based on a predicate
filterf :: (Foldable t) => (a -> Bool) -> t a -> [a]
filterf p = foldr (\x xs -> if p x then x:xs else xs) []