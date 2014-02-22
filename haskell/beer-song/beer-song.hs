module Beer (sing, verse) where
    
verse :: Int -> String
verse 0 = (verseOne "No more bottles") ++
         (verseTwo "Go to the store and buy some more" "99 bottles")
verse 1 = (verseOne "1 bottle") ++
         (verseTwo "Take it down and pass it around" "no more bottles")
verse n | n < 100 && n > 1 = (verseOne ((show n) ++ " bottles")) ++ 
         (verseTwo "Take one down and pass it around" ((show (n-1)) ++ " bottles"))
      

verseOne x = x ++ " of beer on the wall, " ++ x ++ " of beer.\n"
verseTwo x y = x ++ ", " ++ y ++ " of beer on the wall.\n"


sing :: Int -> Int -> String
sing s e = undefined