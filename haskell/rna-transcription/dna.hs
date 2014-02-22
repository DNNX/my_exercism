module DNA (toRNA) where
	
toRNA :: String -> String
toRNA = map f where
    f 'G' = 'C'
    f 'C' = 'G'
    f 'T' = 'A'
    f 'A' = 'U'
    f _ = error "unknown"