{-# language OverloadedStrings #-}
module Network.IPLD.Test where

import Network.IPLD.Example
import Network.IPLD

import Data.Binary.Serialise.CBOR
import Data.ByteString.Lazy (toStrict)
import Data.ByteString (ByteString)
import Test.Tasty
import Test.Tasty.HUnit

runTests :: IO ()
runTests = defaultMain tests

-- "put to /dev/null" (get cid)
putNull :: IsIpld a => a -> Cid
putNull = mkCid . toStrict . serialise . toIpld

putNullB :: IsIpld a => a -> ByteString
putNullB = compact . putNull

tests :: TestTree
tests = testGroup "ipld"
  [ testGroup "serialization"
    [ testCase "1" $ putNullB fooBazExample @=? "zdpuAsgFQS2xim3mRqRTjtZGEHoqMdeoFAehG68CfLuVSjySF"
    ]
  ]
