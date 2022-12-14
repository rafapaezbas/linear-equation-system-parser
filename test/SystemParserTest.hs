module Main where

import Text.Megaparsec
import Test.HUnit
import System.Exit
import SystemParser

instance Eq Term where
  (==) a b = (_coefficient a == _coefficient b) && (_unknown a == _unknown b)
instance Eq Equation where
  (==) a b = (_terms a == _terms b) && (_value a == _value b)

emptyTerm :: Term
emptyTerm = Term 0 ' '

emptyEquation :: Equation
emptyEquation = Equation [] 0

parseTerm :: String -> Term
parseTerm s = case (parse term "" s) of Left e -> emptyTerm
                                        Right t -> t

parseEquation :: String -> Equation
parseEquation s = case (parse equation "" s) of Left e -> emptyEquation
                                                Right e -> e

-- Terms

testA :: Test
testA = TestCase (assertEqual "Terms equality" (Term 1 'x') (Term 1 'x'))

testB :: Test
testB = TestCase (assertEqual "Terms equality (negative)" (Term (-1) 'z') (Term (-1) 'z'))

testC :: Test
testC = TestCase (assertEqual "Valid term" (parseTerm "+3x") (Term 3 'x'))

testD :: Test
testD = TestCase (assertEqual "Valid term (different number)" (parseTerm "+1y") (Term 1 'y'))

testE :: Test
testE = TestCase (assertEqual "Valid term (double digit)" (parseTerm "+11z") (Term 11 'z'))

testF :: Test
testF = TestCase (assertEqual "Valid term (negative)" (parseTerm "-11a") (Term (-11) 'a'))

testG :: Test
testG = TestCase (assertEqual "Invalid term (missing sign)" (parseTerm "11a") emptyTerm)

testH :: Test
testH = TestCase (assertEqual "Invalid term (missing unknown)" (parseTerm "+1") emptyTerm)

testI :: Test
testI = TestCase (assertEqual "Invalid term (missing coefficient)" (parseTerm "+x") emptyTerm)

-- Equations

testJ :: Test
testJ = TestCase (assertEqual "Valid equation (one term)" (parseEquation "+1x=3;") (Equation [(Term 1 'x')] 3))

testK :: Test
testK = TestCase (assertEqual "Valid equation (two terms)" (parseEquation "+1x-2y=3;") (Equation [(Term 1 'x'), (Term (-2) 'y')] 3))

testL :: Test
testL = TestCase (assertEqual "Invalid equation (one terms)" (parseEquation "x-2y=3;") emptyEquation)

testM :: Test
testM = TestCase (assertEqual "Invalid equation (one terms)" (parseEquation "1x2y=3;") emptyEquation)

tests :: Test
tests = TestList [TestLabel "testA" testA,
                  TestLabel "testB" testB,
                  TestLabel "testC" testC,
                  TestLabel "testD" testD,
                  TestLabel "testE" testE,
                  TestLabel "testF" testF,
                  TestLabel "testG" testG,
                  TestLabel "testH" testH,
                  TestLabel "testI" testI,
                  TestLabel "testJ" testJ,
                  TestLabel "testK" testK,
                  TestLabel "testL" testL,
                  TestLabel "testM" testM]

main :: IO ()
main = do
    result <- runTestTT tests
    if (errors result + failures result == 0)
        then exitSuccess
        else exitFailure

