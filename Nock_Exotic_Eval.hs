{- Noun Reduction with Optimization -}
module Nock_Exotic_Eval (nock') where
import Nock_Type

-- While "Nock_Eval.hs" attempts to mimic the nock spec, this evaluation was designed for speed, not beauty

-- Nock Operators --
nockQ' :: Noun -> Noun
nockQ' (a :& b) = (A 0)
nockQ' (A a) = (A 1)
nockQ' (Inf a) = Inf a

nockP' :: Noun -> Noun
nockP' (a :& b) = Inf (a :& b) -- Infinite recurse in spec
nockP' (A a) = (A (1 + a))
nockP' (Inf a) = Inf a

nockE' :: Noun -> Noun
nockE' (a :& b) = if a == b then (A 0) else (A 1)
nockE' (A a) = Inf (A a) -- Infinite recurse in spec
nockE' (Inf a) = Inf a

nockT' :: Noun -> Noun
nockT' ((A 1) :& a) = a
nockT' ((A 2) :& a :& b) = a
nockT' ((A 3) :& a :& b) = b
nockT' ((A n) :& b)
  | n `mod` 2 == 0 = nockT' ((A 2) :& (nockT' ((A (n `div` 2)) :& b)))
  | n `mod` 2 /= 0 = nockT' ((A 3) :& (nockT' ((A ((n-1) `div` 2)) :& b)))
nockT' (A a) = Inf (A a) -- Infinite recurse in spec
nockT' (Inf a) = Inf a


-- Optimized Nock Function --
nock' :: (Noun -> Noun) -> Noun -> Noun
nock' n (a :& (b :& c) :& d) = ((n (a :& b :& c)) :& (n (a :& d)))
nock' _ (a :& (A 0) :& b) = nockT' (b :& a) 
nock' _ (a :& (A 1) :& b) = b
nock' n (a :& (A 2) :& b :& c) = n (n (a :& b) :& (n (a :& c))) -- possiblity for parallel eval here
nock' n (a :& (A 3) :& b) = nockQ' $ n (a :& b)
nock' n (a :& (A 4) :& b) = nockP' $ n (a :& b)
nock' n (a :& (A 5) :& b) = nockE' $ n (a :& b)
nock' n (a :& (A 6) :& b :& c :& d) -- if n (a :& b) doesn't equal 0 or 1 crashes, possible fix would be to return Inf
  | n (a :& b) == (A 0) = n (a :& c)
  | n (a :& b) == (A 1) = n (a :& d)
nock' n (a :& (A 7) :& b :& c) = n ((n (a :& b)) :& c)
nock' n (a :& (A 8) :& b :& c) = n (a :& (A 7) :& (((A 7) :& ((A 0) :& (A 1)) :& b) :& (A 0) :& (A 1)) :& c)
nock' n (a :& (A 9) :& b :& c) = n (a :& (A 7) :& c :& (A 2) :& ((A 0) :& (A 1)) :& (A 0) :& b)
nock' n (a :& (A 10) :& (b :& c) :& d) = n (a :& (A 8) :& c :& (A 7) :& ((A 0) :& (A 3)) :& d)
nock' n (a :& (A 10) :& b :& c) = n (a :& c)
nock' _ (Inf a) = Inf a
nock' _ ((Inf a) :& b) = Inf a
nock' _ (a :& (Inf b)) = Inf b
nock' _ a = Inf a -- Infinite recurse in spec
