{-# language OverloadedStrings #-}
module Network.IPLD.Test where

import Network.IPLD.Example
import Network.IPLD

import Data.Binary.Serialise.CBOR
import Data.ByteString.Lazy (toStrict)
import Data.ByteString (ByteString)
import Data.Text (Text)
import Test.Tasty
import Test.Tasty.HUnit

runTests :: IO ()
runTests = defaultMain tests

-- "put to /dev/null" (get cid)
putNull :: IsIpld a => a -> Cid
putNull = mkCid . toStrict . serialise . toIpld

putNullC :: IsIpld a => a -> ByteString
putNullC = compact . putNull

putNullH :: IsIpld a => a -> Text
putNullH = human . putNull

tests :: TestTree
tests = testGroup "ipld"
  [ testGroup "serialization"
    [ testCase "1" $ putNullC fooBazExample @?= "zdpuAsgFQS2xim3mRqRTjtZGEHoqMdeoFAehG68CfLuVSjySF"
    , testCase "2" $ putNullH fooBazExample @?= "base58btc-cidv1-dag-cbor-sha2-256-32-??"
    ]
  ]
