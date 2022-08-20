module Main where

import SystemParser

strip :: String -> String
strip [] = []
strip s = [c | c <- s, c /= ' ' ]

--run :: String -> Either (ParseErrorBundle String Void) [Equation]
--run s = parse (many equation) "" $ strip $ s

main :: IO ()
main = do
  putStrLn "Helllo, Haskell!"
