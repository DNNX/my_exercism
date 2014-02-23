module Beer (sing, verse) where

import Data.Char (toUpper)    

verse :: Int -> String
verse n = (capitalize $ whatNow n) ++ ".\n"  ++ whatToDoWith n ++ ".\n"

sing :: Int -> Int -> String
sing s e = unlines $ map verse $ [s,(s-1)..e]
  
whatNow :: Int -> String
whatNow n = (bottlize n) ++ beerOnWall ++ ", " ++ (bottlize n) ++ " of beer"

capitalize :: String -> String
capitalize (x:xs) = (toUpper x):xs
capitalize [] = []

whatToDoWith :: Int -> String
whatToDoWith 0 = "Go to the store and buy some more, " ++ (bottlize 99) ++ beerOnWall
whatToDoWith 1 = "Take it down and pass it around, " ++ (bottlize 0) ++ beerOnWall
whatToDoWith n = "Take one down and pass it around, " ++ (bottlize (n-1)) ++ beerOnWall
  
bottlize :: Int -> String    
bottlize 0 = "no more bottles"
bottlize 1 = "1 bottle"
bottlize n = (show n) ++ " bottles"

beerOnWall = " of beer on the wall"