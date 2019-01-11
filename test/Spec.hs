
import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

import Data.List

import Lib

main :: IO ()
--
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [properties, unitTests]

properties :: TestTree
properties = testGroup "Properties" [qcProps]

qcProps :: TestTree
qcProps = testGroup "(checked by QuickCheck)"
  [
    QC.testProperty "sort == sort . reverse" $
        \list -> sort (list :: [Int]) == sort (reverse list)

    , QC.testProperty "Fermat's little theorem" $
        \x -> ((x :: Integer)^7 - x) `mod` 7 == 0

    -- the following property does not hold
    , QC.testProperty "Verify that list == myReverse(myReverse xs))" $
            \xs -> (xs :: [Integer]) == (myReverse $ myReverse xs)

    , QC.testProperty "Do this other thing" $
            \xs -> (xs :: [Integer]) == (myReverse $ myReverse xs)
  ]



unitTests :: TestTree
unitTests = testGroup "Unit Tests"
  [ testCase "Single reverse comparison" $
    [1] `compare` (myReverse [1]) @?= EQ

    , testCase "Empty reverse comparison" $
    ([] :: [Int]) `compare` (myReverse []) @?= EQ
  ]

