
{-

Spec:

nock(a)          *a
[a b c]          [a [b c]]

?[a b]           0
?a               1
+[a b]           +[a b]
+a               1 + a
=[a a]           0
=[a b]           1
=a               =a

/[1 a]           a
/[2 a b]         a
/[3 a b]         b
/[(a + a) b]     /[2 /[a b]]
/[(a + a + 1) b] /[3 /[a b]]
/a               /a

*[a [b c] d]     [*[a b c] *[a d]]

*[a 0 b]         /[b a]
*[a 1 b]         b
*[a 2 b c]       *[*[a b] *[a c]]
*[a 3 b]         ?*[a b]
*[a 4 b]         +*[a b]
*[a 5 b]         =*[a b]

*[a 6 b c d]     *[a 2 [0 1] 2 [1 c d] [1 0] 2 [1 2 3] [1 0] 4 4 b]
*[a 7 b c]       *[a 2 b 1 c]
*[a 8 b c]       *[a 7 [[7 [0 1] b] 0 1] c]
*[a 9 b c]       *[a 7 c 2 [0 1] 0 b]
*[a 10 [b c] d]  *[a 8 c 7 [0 3] d]
*[a 10 b c]      *[a c]

*a               *a

-}


-- Noun Type --

-- [a [b c] 5] == (a :& (b :& c) :& (A 5))
-- Inf acts as starting nub in parsing and infinity in eval, such as in: nockE (A a)
data Noun = A Integer | Noun :& Noun | Inf deriving (Show,  Eq)
infixr 6 :&
cell :: Noun -> Noun -> Noun -- prefix cell constructer with inf identity, used in parse
cell x Inf = x
cell x y = (x :& y) 
nock  :: Noun -> Noun
nockT :: Noun -> Noun
nockQ :: Noun -> Noun
nockP :: Noun -> Noun
nockE :: Noun -> Noun


-- Nock Spec Implementation --

nockQ (a :& b)                      = (A 0)
nockQ (A a)                         = (A 1)
nockQ Inf                           = Inf
nockP (a :& b)                      = Inf -- Infinite
nockP (A a)                         = (A (1 + a))
nockP Inf                           = Inf
nockE (a :& b)                      = if a == b then (A 0) else (A 1)
nockE (A a)                         = Inf -- Infinite
nockE Inf                           = Inf

nockT ((A 1) :& a)                  = a
nockT ((A 2) :& a :& b)             = a
nockT ((A 3) :& a :& b)             = b
nockT ((A n) :& b)
  | n `mod` 2==0                    = nockT ((A 2) :& (nockT ((A (n `div` 2)) :& b)))
  | n `mod` 2/=0                    = nockT ((A 3) :& (nockT ((A ((n-1) `div` 2)) :& b)))
nockT (A a)                         = Inf -- Infinite
nockT Inf                           = Inf

nock (a :& (b :& c) :& d)           = ((nock (a :& b :& c)) :& (nock (a :& d)))

nock (a :& (A 0) :& b)              = nockT (b :& a) 
nock (a :& (A 1) :& b)              = b
nock (a :& (A 2) :& b :& c)         = nock (nock (a :& b) :& (nock (a :& c)))
nock (a :& (A 3) :& b)              = nockQ $ nock (a :& b)
nock (a :& (A 4) :& b)              = nockP $ nock (a :& b)
nock (a :& (A 5) :& b)              = nockE $ nock (a :& b)

nock (a :& (A 6) :& b :& c :& d)    = nock (a :& (A 2) :& ((A 0) :& (A 1)) :& (A 2) :& ((A 1) :& c :& d) :& ((A 1) :& (A 0)) :& (A 2) :& ((A 1) :& (A 2) :& (A 3)) :& ((A 1) :& (A 0)) :& (A 4) :& (A 4) :& b) -- Cuthulu's Reduction
nock (a :& (A 7) :& b :& c)         = nock (a :& (A 2) :& b :& (A 1) :& c)
nock (a :& (A 8) :& b :& c)         = nock (a :& (A 7) :& (((A 7) :& ((A 0) :& (A 1)) :& b) :& (A 0) :& (A 1)) :& c)
nock (a :& (A 9) :& b :& c)         = nock (a :& (A 7) :& c :& (A 2) :& ((A 0) :& (A 1)) :& (A 0) :& b)
nock (a :& (A 10) :& (b :& c) :& d) = nock (a :& (A 8) :& c :& (A 7) :& ((A 0) :& (A 3)) :& d)
nock (a :& (A 10) :& b :& c)        = nock (a :& c)

nock Inf                            = Inf 
nock (Inf :& a)                     = Inf 
nock (a :& Inf)                     = Inf 
nock a                              = Inf -- Infinite


-- Parse, Format, and Main Functions --

parse :: [String] -> (Noun, [String])
parse [] = (Inf, [])
parse ("[":xs) = (cell y z, zs)
  where forwardP = parse xs
        y = fst forwardP
        forwardP2 = parse (snd forwardP)
        z = fst forwardP2
        zs = snd forwardP2
parse ("]":xs) = (Inf, xs)
parse (x:xs)   = (cell (A (read x :: Integer)) y, ys)
  where forwardP = parse xs
        y = fst forwardP
        ys = snd forwardP 

parseNock :: String -> Noun
parseNock x = fst $ parse readyToParse
  where leftBrackets = foldr (\letter seed -> if letter == '[' then (seed++" [ ") else (seed++[letter])) ""
        rightBrackets = foldr (\letter seed -> if letter == ']' then (seed++" ] ") else (seed++[letter])) ""
        readyToParse = (words . rightBrackets . leftBrackets) x

formatNock :: Noun -> String
formatNock (a :& b) = "[ "++(formatNock a) ++ (formatNock b)++"] "
formatNock (A a) = (show a)++" "
formatNock Inf = "∞"

main :: IO()
main = do
  inNock <- getLine
  putStr $ ((\x->x++"\n") . formatNock . nock . parseNock) inNock
  main

  
