{- Noun Type -}
module Nock_Type (Noun(A, (:&), Inf), cell) where


-- [a [b c] 5] == (a :& (b :& c) :& (A 5))
-- Inf acts as starting nub in parsing and infinity in eval, such as in: nockE (A a); like Nothing type in Maybe monad
data Noun = A Integer | Noun :& Noun | Inf deriving (Show,  Eq)
infixr 6 :&


-- Prefix cell constructer with inf identity, used in parse
cell :: Noun -> Noun -> Noun
cell x Inf = x
cell x y = (x :& y)
