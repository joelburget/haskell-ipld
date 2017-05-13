{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Main (main) where

import           Control.Applicative
import qualified Control.Foldl as Fold
import qualified Data.Aeson as Aeson
import qualified Data.Attoparsec.ByteString as ABS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString as SBS
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Map as Map
import           Data.Text (Text)
import qualified Data.Vector as V
import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import           Data.Binary.Serialise.CBOR (serialise, deserialise)
import           Turtle
import qualified Turtle.Bytes as TB
import           Data.Scientific
import           System.Exit (exitFailure)

import Network.IPLD
import Network.IPLD.Client

genTextValue :: Monad m => Gen m Text
genTextValue = Gen.text (Range.linear 0 100) Gen.unicode

genScientific :: Monad m => Gen m Scientific
genScientific = scientific
  <$> (toInteger <$> Gen.int64 Range.linearBounded)
  <*> Gen.int Range.linearBounded

genValue :: Monad m => Gen m Value
genValue
    = TextValue              <$> genTextValue
  <|> LinkValue . MerkleLink <$> genCid
  <|> DagArray  . V.fromList <$> Gen.list (Range.linear 0 10) genValue
  <|> DagObject              <$> genDagObject (Range.linear 0 10)
  <|> DagNumber              <$> genScientific
  <|> DagBool                <$> Gen.bool
  <|> pure Null

genDagObject :: Monad m => Range Int -> Gen m (HashMap Text Value)
genDagObject range =
  let pairgen = (,) <$> genTextValue <*> genValue
  in HashMap.fromList . Map.toList <$> Gen.map range pairgen

genCid :: Monad m => Gen m Cid
genCid = mkCid <$> Gen.bytes (Range.linear 100 200)

-- | Traversing an empty path gives the original value.
prop_traverse_empty :: Property
prop_traverse_empty = property $ do
  val <- forAll genValue
  traverseValue (RelMerklePath []) val === Found val

-- | Check that `parse . unparse == id`.
prop_parse_unparse_cid :: Property
prop_parse_unparse_cid = property $ do
  cid <- forAll genCid
  let cidStr = compact cid
      parsed = ABS.parseOnly (parseCid <* ABS.endOfInput) cidStr
  parsed === Right cid

-- | Check that our CID matches the one IPFS generates.
prop_matches_ipfs :: Property
prop_matches_ipfs = property $ do
  -- TODO:
  --   * check for ipfs version (check has dag command)
  --   * only do this once?
  (ExitSuccess, _stdout, _stderr) <- shellStrictWithErr "which ipfs" ""

  value <- forAll genValue

  Right ipfsCid <- liftIO $ put value
  valueCid value === ipfsCid

prop_serialize_round_trip :: Property
prop_serialize_round_trip = property $ do
  value <- forAll genValue
  let value' = deserialise $ serialise value
  value' === value

-- prop_traverse_graft :: Property

-- prop_graft_hash :: Property

main :: IO ()
main = do
  good <- checkParallel $$(discover)
  unless good exitFailure
