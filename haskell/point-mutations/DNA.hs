module DNA (hammingDistance) where
    
hammingDistance :: String -> String -> Int
hammingDistance a b = sum $ zipWith f a b where f x y = if (x == y) then 0 else 1