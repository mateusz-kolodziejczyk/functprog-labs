module Main where

import Lib
import Palindrome.LibPalindrome
import Data.Text.IO as IO

main :: IO ()
main = do
    input <- IO.getLine
    let result = isPalindrome input
    print result