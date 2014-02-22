module Bob (responseFor) where
import Data.Char
	
responseFor :: String -> String
responseFor [] = emptyRes
responseFor ('?':xs) = reQuest xs
responseFor (x:xs)
    | isLower x = reLower xs
    | isUpper x = reUpper xs
    | isDigit x = reDigit xs
responseFor (_:xs) = responseFor xs

reQuest :: String -> String
reQuest []		 = answer
reQuest (' ':xs) = reQuest xs
reQuest ('?':xs) = reQuest xs
reQuest _ 		 = whatever

reDigit :: String -> String
reDigit [] = whatever
reDigit ('?':xs) 		   = reQuest xs
reDigit (x:xs) | isDigit x = reDigit xs
reDigit (_:xs)			   = responseFor xs

reLower :: String -> String
reLower [] = whatever
reLower ('?':xs) = reQuest xs
reLower (_:xs)   = reLower xs

reUpper :: String -> String
reUpper [] = shout
reUpper (x:xs) | isLower x = reLower xs
reUpper (_:xs) 			   = reUpper xs
	
answer = "Sure."	
shout = "Woah, chill out!"
whatever = "Whatever."
emptyRes = "Fine. Be that way!"