
----------------------------------
-- add a type declaration
-- to each of the named expressions
i1:: Integer  -- I have done the first one for you
i1 = 45

i2 = "123"

i3 = 45 <= i1

i4 = 'c'

i5 = ["abc","ok"]

i6 = head i5

i7 = tail "abc"  -- Recall a string is a shorthand for a list of Char

i8 = (True,4.5)

i9 = [i1,34]

-------------------------------------------------
-- For each named expression replace "undefined"
-- with an expression with the same type as the declaration


j1:: (String,Integer)
j1 = undefined

j2:: [Integer]
j2 = undefined

j3:: Char
j3 = undefined


j4:: Double
j4 = undefined


j5:: (Integer,String,Integer,Char)
j5 = undefined

j6:: ([Char],(Bool,String))
j6 = undefined

j7:: [[Bool]]
j7 = undefined

j8:: [(String,Bool)]
j8 = undefined