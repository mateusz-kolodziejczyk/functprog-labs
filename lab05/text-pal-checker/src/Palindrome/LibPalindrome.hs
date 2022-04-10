module Palindrome.LibPalindrome where

import Data.Char ( isSpace, isPunctuation )
import Data.Text as T ( Text, filter, pack, reverse, toLower ) 


isPalindrome :: Text -> Bool
isPalindrome xs =
    cleanS == T.reverse(cleanS)
    where
        cleanS = T.toLower(T.filter (\c -> isSpace c || isPunctuation c) xs)