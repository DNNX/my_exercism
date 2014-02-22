module WordCount (wordCount) where

import           Data.Char
import           Data.List (foldl')
import qualified Data.Map as M
import           Control.Monad (sequence)

wordCount :: String -> M.Map String Int
wordCount = M.fromListWith (+) . map (\s -> (map toLower s, 1)) . toWords

toWords :: String -> [String]
toWords = splitWhen (not . or . sequence [isUpper, isLower, isDigit])

-- split a list by a predicate, skipping empty elements as well
-- could drop the reverse function in the middle, though
splitWhen :: (a -> Bool) -> [a] -> [[a]]
splitWhen predicate list = filter (not . null) $ reverse $ map reverse $ splitWhen' [] predicate list
    where splitWhen' :: [[a]] -> (a -> Bool) -> [a] -> [[a]]
          splitWhen' tada _ [] = tada
          splitWhen' acc p (x:xs) | p x = splitWhen' ([]:acc) p xs
          splitWhen' [] p (x:xs)        = splitWhen' [[x]] p xs
          splitWhen' (y:ys) p (x:xs)    = splitWhen' ((x:y):ys) p xs