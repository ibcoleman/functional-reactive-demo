module AccountSpec (accountTests) where

import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit
import Data.List

import Lib
--import Account
import Account

main :: IO ()
main = defaultMain accountTests

accountTests :: TestTree
accountTests = testGroup "Account Tests" [properties]

-- Define the properties (QuickCheck) tests
properties :: TestTree
properties = testGroup "Properties" [qcProps]

qcProps :: TestTree
qcProps = testGroup "QuickCheck AccountSpec Tests"
  [
    QC.testProperty "sort == sort . reverse" $
        \list -> sort (list :: [Int]) == sort (reverse list)
  ]
