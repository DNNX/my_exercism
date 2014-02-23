module DNA (count, nucleotideCounts) where

import           Data.Maybe (fromMaybe)
import qualified Data.Map as M    
    
count :: Char -> String -> Int
count c s | isNucleotides c = fromMaybe 0 $ M.lookup c (nucleotideCounts s)
count c _ = error $ "invalid nucleotide '" ++ [c] ++ "'"

isNucleotides = (flip elem) "ACGTU"

nucleotideCounts :: String -> M.Map Char Int
nucleotideCounts sqs = M.fromListWith (+) $ (map f sqs ++ zeroid)
                          where f = \c -> (c, 1)
                                zeroid = map (\c -> (c, 0)) "ACGT"
