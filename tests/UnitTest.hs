{-# language DeriveGeneric #-}
{-# language MultiWayIf #-}
{-# language OverloadedStrings #-}
module Main (main) where

import Network.IPLD
import Network.IPLD.Lens

import           Control.Lens hiding ((.=))
import           Codec.CBOR.Encoding
import           Codec.CBOR.Write
import           Data.ByteString (ByteString)
import           Data.ByteString.Lazy (toStrict)
import           Data.Text (Text)
import           GHC.Generics
import           Test.Tasty
import           Test.Tasty.HUnit
import qualified Data.Attoparsec.ByteString as ABS
import qualified Data.HashMap.Strict as H
import qualified Data.Vector as V
import qualified Data.ByteString.Base16 as Hex

main :: IO ()
main = defaultMain tests

-- "put to /dev/null" (get cid)
putNull :: IsIpld a => a -> Cid
putNull = mkCid . toStrictByteString . encodeValue . toIpld

putNullC :: IsIpld a => a -> ByteString
putNullC = compact . putNull

-- putNullH :: IsIpld a => a -> Text
-- putNullH = human . putNull

data Test
  = Alt1
  | Alt2 Int
  | Alt3
    { _field1 :: Text
    , _field2 :: Test
    }
  | Alt4 Int Int Int
  deriving (Generic, Show, Eq)

instance IsIpld Test

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
      let cidStr = "zdpuAnHCHH7uXZ87iMR3ZNx1L9UnDyACnKrxmWQrBVxt9zDVH"

          (expectedBytes, "") = Hex.decode
            "1b9aaba765ec0aeaf79e1ee4cc26782b4259586c9ea1d49f5dfc2e2667ff9420"
          expected = Right $
            Cid Base58Btc 1 DagCbor $ Multihash Sha2_256 32 expectedBytes

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
        let actual = linkM Left root
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


  , let id' :: IsIpld a => a -> Maybe a
        id' = fromIpld . toIpld

        test1, test2, test3, test4, test5 :: Test
        test1 = Alt1
        test2 = Alt2 3
        test3 = Alt3 "joel" Alt1
        test4 = Alt3 "joel" (Alt3 "burget" test1)
        test5 = Alt4 1 2 3
    in testGroup "toIpld / fromIpld"
         [ testCase "toIpld test1" $
             toIpld test1 @?= array [DagNumber 0]
         , testCase "toIpld test2" $
             toIpld test2 @?= array [DagNumber 1, DagNumber 3]
         , testCase "toIpld test3" $
             toIpld test3 @?= array [DagNumber 2, "joel", toIpld test1]
         , testCase "toIpld test4" $
             toIpld test4 @?= array
               [ DagNumber 2
               , TextValue "joel"
               , array [DagNumber 2, "burget", toIpld test1]
               ]
         , testCase "toIpld test5" $
             toIpld test5 @?=
               array [DagNumber 3, DagNumber 1, DagNumber 2, DagNumber 3]

         , testCase "fromIpld . toIpld (test1)" $
             id' test1 @?= Just test1
         , testCase "fromIpld . toIpld (test2)" $
             id' test2 @?= Just test2
         , testCase "fromIpld . toIpld (test3)" $
             id' test3 @?= Just test3
         , testCase "fromIpld . toIpld (test4)" $
             id' test4 @?= Just test4
         , testCase "fromIpld . toIpld (test5)" $
             id' test5 @?= Just test5
         ]

  ]
