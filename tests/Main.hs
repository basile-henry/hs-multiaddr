module Main(main) where

import           Data.Maybe            (fromJust)
import           Network.MultiAddr
import           Test.HUnit            (Assertion, (@?=))
import           Test.Tasty            (TestTree, defaultMain, testGroup,
                                        testGroup)
import           Test.Tasty.HUnit      (testCase)
main :: IO ()
main = defaultMain $ testGroup "Tests" [
        testCase  "testCase01" testCase01,
        testCase  "testCase02" testCase02,
        testCase  "testCase03" testCase03,
        testCase  "testCase04" testCase04,
        testCase  "testCase05" testCase05,
        testCase  "testCase06" testCase06,
        testCase  "testCase07" testCase07,
        testCase  "testCase08" testCase08,
        testCase  "testCase09" testCase09,
        testCase  "testCase10" testCase10
    ]

testCase01 :: Assertion
testCase01 = undefined

testCase02 :: Assertion
testCase02 = undefined

testCase03 :: Assertion
testCase03 = undefined

testCase04 :: Assertion
testCase04 = undefined

testCase05 :: Assertion
testCase05 = undefined

testCase06 :: Assertion
testCase06 = undefined

testCase07 :: Assertion
testCase07 = undefined

testCase08 :: Assertion
testCase08 = undefined

testCase09 :: Assertion
testCase09 = undefined

testCase10 :: Assertion
testCase10 = undefined
