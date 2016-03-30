module Main(main) where

import           Data.ByteString   (pack, unpack)
import           Network.MultiAddr
import           Test.HUnit        (Assertion, (@?=))
import           Test.Tasty        (defaultMain, testGroup)
import           Test.Tasty.HUnit  (testCase)
main :: IO ()
main = defaultMain $ testGroup "Tests" [
        testCase  "testCase01" testCase01,
        testCase  "testCase02" testCase02,
        testCase  "testCase03" testCase03,
        testCase  "testCase04" testCase04,
        testCase  "testCase05" testCase05,
        testCase  "testCase06" testCase06
    ]

testCase01 :: Assertion
testCase01 =
    fromString "ip4/127.0.0.1/tcp/1234" @?= Nothing

testCase02 :: Assertion
testCase02 =
    fromString "/////ip6//::1/////tcp///////1234////" @?= fromString "/ip6/::1/tcp/1234"

testCase03 :: Assertion
testCase03 =
    (decode =<< encode <$> fromString "/ip4/127.0.0.1/tcp/4001/ipfs/Qmd7aqZhb93HVZ5S4tyyF84dTbpN6SmgfdNPYgFB8wUyo8")
    @?= Just [(IP4, pack [127,0,0,1]),(TCP, pack [15,161]),(IPFS, pack [18,32,219,134,96,62,152,31,254,221,182,235,36,19,208,199,247,51,10,254,150,69,194,12,196,254,184,205,0,66,90,25,131,203])]

testCase04 :: Assertion
testCase04 = 
    (unpack . encode <$> fromString "/ip4/127.0.0.1/tcp/4001/ipfs/Qmd7aqZhb93HVZ5S4tyyF84dTbpN6SmgfdNPYgFB8wUyo8")
    @?= Just [4,127,0,0,1,6,15,161,165,3,34,18,32,219,134,96,62,152,31,254,221,182,235,36,19,208,199,247,51,10,254,150,69,194,12,196,254,184,205,0,66,90,25,131,203]

testCase05 :: Assertion
testCase05 =
    (findAddressString IPFS =<< fromString "/ip4/127.0.0.1/tcp/4001/ipfs/Qmd7aqZhb93HVZ5S4tyyF84dTbpN6SmgfdNPYgFB8wUyo8")
    @?= Just "Qmd7aqZhb93HVZ5S4tyyF84dTbpN6SmgfdNPYgFB8wUyo8"

testCase06 :: Assertion
testCase06 =
    (findAddress IPFS =<< fromString "/ip4/127.0.0.1/tcp/4001/ipfs/Qmd7aqZhb93HVZ5S4tyyF84dTbpN6SmgfdNPYgFB8wUyo8")
    @?= (Just $ pack [18,32,219,134,96,62,152,31,254,221,182,235,36,19,208,199,247,51,10,254,150,69,194,12,196,254,184,205,0,66,90,25,131,203])
