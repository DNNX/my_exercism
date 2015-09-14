module DNA (count, nucleotideCounts) where

import qualified Data.Map as M    
    
count :: Char -> String -> Int
count c s | isNucleotides c = M.findWithDefault 0 c (nucleotideCounts s)
          | otherwise       = error $ "invalid nucleotide " ++ show c

isNucleotides :: Char -> Bool
isNucleotides = (flip elem) "ACGTU"

nucleotideCounts :: String -> M.Map Char Int
nucleotideCounts sqs = M.fromListWith (+) $ (map f sqs ++ zeroid)
                          where f = \c -> (c, 1)
                                zeroid = map (\c -> (c, 0)) "ACGT"
