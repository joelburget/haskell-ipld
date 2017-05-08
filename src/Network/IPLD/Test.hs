{-# language OverloadedStrings #-}
module Network.IPLD.Test where

-- import Network.IPLD.Example
import Network.IPLD

import qualified Data.Attoparsec.ByteString as ABS
import Data.Binary.Serialise.CBOR
import Data.ByteString.Lazy (toStrict)
import Data.ByteString (ByteString)
import qualified Data.HashMap.Strict as H
import Data.Text (Text)
import qualified Data.Vector as V
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
  -- [ testGroup "serialization"
  --   [ testCase "1" $
  --       putNullC fooBazExample @?= "zdpuAsgFQS2xim3mRqRTjtZGEHoqMdeoFAehG68CfLuVSjySF"
  --   , testCase "2" $
  --       putNullH fooBazExample @?= "base58btc-cidv1-dag-cbor-sha2-256-32-??"
  --   ]
  [ testGroup "traversal"
    [ testCase "1" $
      let val = TextValue "hello, world"
          expected = Found val
          actual = traverseValue [] val
      in actual @?= expected
    ]
  , testGroup "multihash"
    [ testCase "hello, world" $
      let expected = "zdpuAxhpv1we4dKYKEeXnDF9KoenuFuXoKxuA3t3YbDRay7h4"
          actual = putNullC $ TextValue "hello, world"
      in actual @?= expected
    -- , testCase "hello, world" $
    --     human (mkCid "hello, world\n") @?= ""
    , testCase "['hello', 'world']" $
      let expected = "zdpuAzk5JJeWob78jLxf3Hsed1iPaZX4U4bBuahjZoa6PXbtV"
          actual = putNullC $ DagArray $ V.fromList
            [ TextValue "hello"
            , TextValue "world"
            ]
      in actual @?= expected
    -- TODO: convert to numbers when we have them
    , testCase "{ \"x\": \"1\", \"y\": \"2\" }" $
      let expected = "zdpuAnHCHH7uXZ87iMR3ZNx1L9UnDyACnKrxmWQrBVxt9zDVH"
          actual = putNullC $ DagObject $ H.fromList
            [ ("x", TextValue "1")
            , ("y", TextValue "2")
            ]
      in actual @?= expected
    -- , testCase "link" $
    --   let expected = "dpuAu19hy6vVmfjcR3zW5dHRmte9RZXgMmWnr8wcmnbFmR2E"
    --       actual = putNullC $ LinkValue $ MerkleLink helloWorldAddr
    --   in actual @?= expected
    ]

  , testGroup "parsing"
    [ testCase "parse cid" $
      let expected = Right $
            Cid Base58Btc 1 DagCbor $ Multihash Sha2_256 32
              "QmQCQWTmBKvTqY8xRo3RsXfS1iiXriinYKVCY5eSUE8zaT"
            -- mkCid "uAnHCHH7uXZ87iMR3ZNx1L9UnDyACnKrxmWQrBVxt9zDVH"
          cidStr = "zdpuAnHCHH7uXZ87iMR3ZNx1L9UnDyACnKrxmWQrBVxt9zDVH"
          actual = ABS.parseOnly (parseCid <* ABS.endOfInput) cidStr
      in actual @?= expected
    ]

  ]
