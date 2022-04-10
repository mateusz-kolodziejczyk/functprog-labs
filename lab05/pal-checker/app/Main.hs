module Main where

import Lib
import Palindrome.LibPalindrome

main :: IO ()
main = do
    input <- getLine
    let result = isPalindrome input
    print result