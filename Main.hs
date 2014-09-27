{- Main Nock Module -}
import System.Environment
import Nock_Parse
import Nock_Eval


-- Parses stdin as nock, evaluates, then outputs formated nock to stdout --
-- Note: no jet support yet
main :: IO()
main = do
  args <- getArgs
  if args /= [] && (head . last) args /= '-'
  then do
    file <- readFile $ last args
    putStr $ (showNoun . nock . readNoun) file
  else do
    inNock <- getLine
    if inNock /= ":q" 
    then do
      putStr $ ((\x->x++"\n") . showNoun . nock . readNoun) inNock
      main
    else return ()
