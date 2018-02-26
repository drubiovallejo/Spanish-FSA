-- David Rubio Vallejo

-- This project captures the spirantization that voiced
-- stops undergo in Spanish when in between vowels or after a liquid and a vowel.

-- G is the voiced velar fricative
-- B is the voiced bilabial fricative
-- D is the voiced labiodental fricative

import Dfsa

truewords = ["aGua", "aBion", "daDo" , "saBana", "ronda", "gorDo", "salBo",
 "barDo", "barBa", "mango", "embarGo", "galGo", "signo"]

falsewords = ["agua", "Dado", "gordo", "ronDa", "salbo", "manGo",
 "emBargo", "GorDo", "siGno" ]
 
 
voicedStops :: String
voicedStops = "bdg"
 
spirantizedCons :: String
spirantizedCons = "BDG"
 
liquids :: String
liquids = "rl"
 
otherCons :: String
otherCons = "cfhjkmnpqstvxz"
 
vowels :: String
vowels = "aeiou"
 
 
spanishLenition :: DFSA String Char
spanishLenition = A qs bs q0 fs d
 where
 qs = ["live","sink","just saw V or liquid", "just saw spirantizedCons", "just saw non-spirantizedCons after V or liquid"]
 bs = voicedStops ++ spirantizedCons ++ liquids ++ otherCons ++ vowels 
 q0 = "live"
 fs = ["live","just saw V or liquid", "just saw non-spirantizedCons after V or liquid"]
 d = dSpirant

dSpirant :: String -> Char -> String 

dSpirant "sink" _ = "sink"

dSpirant "live" b
 | elem b liquids = "just saw V or liquid"
 | elem b vowels = "just saw V or liquid"
 | elem b spirantizedCons = "sink"
 | otherwise = "live"

dSpirant "just saw V or liquid" b
 | elem b spirantizedCons = "just saw spirantizedCons"
 | elem b liquids = "just saw V or liquid"
 | elem b vowels = "just saw V or liquid"
 | otherwise = "just saw non-spirantizedCons after V or liquid"

dSpirant "just saw spirantizedCons" b
 | elem b vowels = "live"
 | otherwise = "sink"
 
dSpirant "just saw non-spirantizedCons after V or liquid" b
 | elem b vowels = "sink"
 | elem b spirantizedCons = "sink"
 | otherwise = "live"
 
 
 
-- Here are some Input / Output commands. This looks more imperative
-- because we are not just calculating values. We are showing
-- something to the screen.

test :: String -> IO ()
test w = 
 let b = recognize spanishLenition w 
 in putStrLn (show w ++ "\t\t" ++ show b)   -- "\t" is a tab

testWords = truewords ++ falsewords

-- execute takes a function and a list and DOES SOME IO. The function
-- takes an argument of type a and DOES SOME I/O WITH IT. Execute runs
-- the function over the elements in the list in sequence, applying f
-- to each element in the list.
execute :: (a -> IO()) -> [a] -> IO () 
execute _ [] = putStrLn "Done!"
execute f (x:xs) = do f x ; execute f xs

main = do execute test testWords