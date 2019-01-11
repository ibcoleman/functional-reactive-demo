import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit
import Data.List

import Lib
--import Account
import Account
import AccountSpec

main :: IO ()
main = defaultMain tests

-- Define the root TestTree
tests :: TestTree
tests = testGroup "Tests" [properties, unitTests]

-- Define the properties (QuickCheck) tests
properties :: TestTree
properties = testGroup "Properties" [qcProps, accountTests]

qcProps :: TestTree
qcProps = testGroup "Spec Tests"
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

aMoneyValue :: Money
aMoneyValue = Money 7.83

bMoneyValue :: Money
bMoneyValue = Money 100


aTransactionAmount :: Amount
aTransactionAmount = Amount { ta = Money 100}

anAccountBalance :: Balance
anAccountBalance = Balance { ba = Money 96 }

unitTests :: TestTree
unitTests = testGroup "Unit Tests"
  [ testCase "Single reverse comparison" $
    [(1 :: Integer)] `compare` (myReverse [1]) @?= EQ

    , testCase "Empty reverse comparison" $
    ([] :: [Int]) `compare` (myReverse []) @?= EQ

    , testCase "Adding and subtracting float numbers" $
    (aMoneyValue `compare` (bMoneyValue + aMoneyValue + aMoneyValue - bMoneyValue - aMoneyValue)) @?=EQ

    , testCase "Check transaction amount" $
    (Amount {ta = Money 100}) `compare` aTransactionAmount @?= EQ

    , testCase "Deduct transaction amount" $
    (Amount {ta = Money 100}) `compare` aTransactionAmount @?= EQ
  ]

