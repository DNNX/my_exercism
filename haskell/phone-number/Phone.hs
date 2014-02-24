module Phone (areaCode, number, prettyPrint) where
    
import Data.Char (isDigit)
    
number :: String -> String
number = nb . filter isDigit
         where nb xs       | length xs == 10 = xs
               nb ('1':xs) | length xs == 10 = xs
               nb _                          = badNumber

areaCode :: String -> String
areaCode = take 3 . number

prettyPrint :: String -> String
prettyPrint str = let (area, parts) = splitAt 3 (number str)
                      (partA, partB) = splitAt 3 parts
                  in "(" ++ area ++ ") " ++ partA ++ "-" ++ partB

badNumber = take 10 $ cycle "0"