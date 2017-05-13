{-# language OverloadedStrings #-}
{-# language MultiWayIf #-}
module Network.IPLD.Test (runTests) where

import Network.IPLD
import Network.IPLD.Lens

import           Control.Lens hiding ((.=))
import           Data.Binary.Serialise.CBOR
import           Data.ByteString (ByteString)
import           Data.ByteString.Lazy (toStrict)
import           Data.Text (Text)
import           Test.Tasty
import           Test.Tasty.HUnit
import qualified Data.Attoparsec.ByteString as ABS
import qualified Data.HashMap.Strict as H
import qualified Data.Vector as V

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
          actual = traverseValue (RelMerklePath []) val
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
    --   let expected = "zdpuAu19hy6vVmfjcR3zW5dHRmte9RZXgMmWnr8wcmnbFmR2E"
    --       helloWorldAddr = "zdpuAxhpv1we4dKYKEeXnDF9KoenuFuXoKxuA3t3YbDRay7h4"
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

  , testGroup "manipulation" $
    let childA = "hi, i'm child a"
        childB = object
          [ "info" .= "hi, i'm child b"
          , "link" .= linkToV childC
          ]
        childC = object
          [ "info" .= "hi, i'm child c"
          , "link" .= linkToV childD
          ]
        childD = "hi, i'm child d"
        root = object
          [ "link a" .= linkToV childA
          , "link b" .= linkToV childB
          ]

        linker lnk = if
          | lnk == linkToM childA -> childA
          | lnk == linkToM childB -> childB
          | lnk == linkToM childC -> childC
          | lnk == linkToM childD -> childD
          | otherwise -> error "bad link"
    in

      [ testCase "replacing a single link" $
        let actual = root & key "link a" .~ childA
            expected = object
              [ "link a" .= childA
              , "link b" .= linkToV childB
              ]
        in actual @?= expected

      , testCase "replacing many links, non-recursively" $
        let actual = graft linker root
            expected = object
              [ "link a" .= childA
              , "link b" .= childB
              ]
        in actual @?= expected

      , testCase "replacing many links, recursively" $
        let actual = link linker root
            expected = object
              [ "link a" .= childA
              , "link b" .= object
                [ "info" .= "hi, i'm child b"
                , "link" .= object
                  [ "info" .= "hi, i'm child c"
                  , "link" .= childD
                  ]
                ]
              ]
        in actual @?= expected

      , testCase "erroring when we can't find a link" $
        let f = Left
            actual = linkM f root
            expected = Left (linkToM childA)
        in actual @?= expected

      , testCase "ignoring when we can't find a link" $
        let f lnk = if
              | lnk == linkToM childA -> childA
              | otherwise             -> LinkValue lnk
            actual = graft f root
            expected = object
              [ "link a" .= childA
              , "link b" .= linkToV childB
              ]
        in actual @?= expected
      ]

  ]
