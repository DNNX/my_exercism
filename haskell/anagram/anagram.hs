module Anagram (anagramsFor) where

import Data.List (sort)
import Data.Char (toLower)
    
anagramsFor :: String -> [String] -> [String]
anagramsFor str = filter (isAnagramOf str)
          
isAnagramOf :: String -> String -> Bool
isAnagramOf a b | length b /= length a = False
                | a == b               = False
                | otherwise = f a == f b
                              where f = sort . map toLower