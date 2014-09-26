{- Main Nock Module -}
import Nock_Parse
import Nock_Eval


-- Parses stdin as nock, evaluates, then outputs formated nock to stdout --
-- Note: no jet support yet --
main :: IO()
main = do
  inNock <- getLine
  if inNock /= ":q" 
  then do
    putStr $ ((\x->x++"\n") . showNoun . nock . readNoun) inNock
    main
  else return ()
