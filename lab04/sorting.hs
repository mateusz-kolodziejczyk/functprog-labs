import Data.List ( sort, sortBy )
names :: [(String, String)]
names = [("Mark", "Biden"), ("Angela", "Merkel"), ("Joe", "Biden"), ("Michael D", "Higgins"), ("Boris", "Johnson")]

compareFirstNames :: Ord a => (a, a) -> (a, a) -> Ordering
compareFirstNames name1 name2 
            | firstName1 > firstName2 = GT
            | firstName1 < firstName2 = LT
            | otherwise = EQ
    where   firstName1 = fst name1
            firstName2 = fst name2



compareLastNames :: Ord a => (a, a) -> (a, a) -> Ordering
compareLastNames name1 name2 
           | lastName1 > lastName2 = GT 
           | lastName1 < lastName2 = LT
           | otherwise = compareFirstNames name1 name2
    where   lastName1 = snd name1
            lastName2 = snd name2
