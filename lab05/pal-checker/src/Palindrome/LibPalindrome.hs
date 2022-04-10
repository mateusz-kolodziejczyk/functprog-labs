module Palindrome.LibPalindrome where

isPalindrome :: String -> Bool
isPalindrome xs = xs == reverse(xs)
