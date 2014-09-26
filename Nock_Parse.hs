{- Noun Parsing & Printing -}
module Nock_Parse (readNoun, showNoun) where
import Nock_Type


-- Parsing function --
parse :: [String] -> (Noun, [String])
parse [] = (Inf, [])
parse ("[":xs) = (cell cellHead cellTail, tailRemainder)
  where xsParsed = parse xs
        cellHead = fst xsParsed
        tailParse = parse (snd xsParsed)
        cellTail = fst tailParse
        tailRemainder = snd tailParse
parse ("]":xs) = (Inf, xs)
parse (x:xs)   = (cell (A (read x :: Integer)) cellHead, parseRemainder)
  where xsParsed = parse xs
        cellHead = fst xsParsed
        parseRemainder = snd xsParsed
         

-- Noun Read function, formats string then applies parse --
readNoun :: String -> Noun
readNoun x = fst $ parse readyToParse
  where leftBrackets = foldr (\letter seed -> if letter == '[' then (seed++" [ ") else (seed++[letter])) ""
        rightBrackets = foldr (\letter seed -> if letter == ']' then (seed++" ] ") else (seed++[letter])) ""
        readyToParse = (words . rightBrackets . leftBrackets) x


-- Noun Show function --
-- Note: currently outputs proper nock, [1 [2 [3 4]]], instead of standard nock, [1 2 3 4]
showNoun :: Noun -> String
showNoun (a :& b) = "[ "++(showNoun a) ++ (showNoun b)++"] "
showNoun (A a) = (show a)++" "
showNoun Inf = "∞"
