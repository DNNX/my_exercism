module Anagram (anagramsFor) where

import Data.List (sort)
import Data.Char (toLower)
    
anagramsFor :: String -> [String] -> [String]
anagramsFor str = filter isAnagram
                  where f           = sort . map toLower
                        canonical   = f str
                        len         = length str
                        isAnagram s | length s /= len = False
                                    | s == str        = False
                                    | otherwise       = canonical == f s
