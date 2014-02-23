module WordCount (wordCount) where

import           Data.Char (isAlphaNum, toLower)
import           Data.List.Split (wordsBy)
import qualified Data.Map as M

wordCount :: String -> M.Map String Int
wordCount = M.fromListWith (+) . map (\s -> (map toLower s, 1)) . wordsBy (not.isAlphaNum)