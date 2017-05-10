{-# LANGUAGE TemplateHaskell #-}
module Network.IPLD.Hedgehog where

import           Control.Applicative
import qualified Data.Attoparsec.ByteString as ABS
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Map as Map
import           Data.Text (Text)
import qualified Data.Vector as V
import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import           Data.Binary.Serialise.CBOR

import Network.IPLD.Cid
import Network.IPLD.Internal

genTextValue :: Monad m => Gen m Text
genTextValue = Gen.text (Range.linear 0 100) Gen.unicode

genValue :: Monad m => Gen m Value
genValue
    = TextValue              <$> genTextValue
  <|> LinkValue . MerkleLink <$> genCid
  <|> DagArray  . V.fromList <$> Gen.list (Range.linear 0 10) genValue
  <|> DagObject              <$> genDagObject (Range.linear 0 10)

genDagObject :: Monad m => Range Int -> Gen m (HashMap Text Value)
genDagObject range =
  let pairgen = (,) <$> genTextValue <*> genValue
  in HashMap.fromList . Map.toList <$> Gen.map range pairgen

genCid :: Monad m => Gen m Cid
genCid = mkCid <$> Gen.bytes (Range.linear 100 200)

prop_traverse_empty :: Property
prop_traverse_empty = property $ do
  val <- forAll genValue
  traverseValue [] val === Found val

prop_parse_unparse_cid :: Property
prop_parse_unparse_cid = property $ do
  -- rawHash <- Gen.bytes (Range.constant 32)
  -- let multihash = Multihash Sha2_256 32 rawHash
  --     cid = Cid Base58Btc 1 DagCbor multihash
  cid <- forAll genCid
  let cidStr = compact cid
      parsed = ABS.parseOnly (parseCid <* ABS.endOfInput) cidStr
  parsed === Right cid

-- prop_matches_ipfs :: Property
-- prop_matches_ipfs = property $ do
--   -- TODO: check for ipfs version
--   hasIpfs <- shell "which ipfs" ""
--   when (hasIpfs /= ExitSuccess) (throwError "can't find ipfs executable")

--   value <- forAll genValue
--   ipfsHash <- shell "ipfs dag put -" XXX
--   let ourHash = mkCid value
--   ourHash === ipfsHash

prop_serialize_round_trip :: Property
prop_serialize_round_trip = property $ do
  value <- forAll genValue
  let value' = deserialise $ serialise value
  value' === value

tests :: IO Bool
tests = checkParallel $$(discover)
