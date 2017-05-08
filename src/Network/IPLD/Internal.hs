{-# language BangPatterns #-}
{-# language DeriveGeneric #-}
{-# language LambdaCase #-}
{-# language OverloadedStrings #-}

module Network.IPLD.Internal where

import Prelude hiding (takeWhile)

import Control.Applicative
import Control.Lens ((^?), ix)
import Data.HashMap.Strict (HashMap)
import Data.Monoid ((<>))
import Data.String
import Data.Text (Text)
import Data.Vector (Vector)
import GHC.Generics
import Text.Read
import Data.ByteString.Base58
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString as BS
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as T
import qualified Data.Vector as V

import           Data.Attoparsec.Text
import qualified Data.Attoparsec.ByteString as ABS

import           Data.Binary.Serialise.CBOR.Class
import           Data.Binary.Serialise.CBOR.Encoding
import           Data.Binary.Serialise.CBOR.Decoding

import Network.IPLD.Cid

-- $coding
--
-- The key here is adhering to the cbor standard specified in the ipld spec

cborTagLink :: Word
cborTagLink = 42

newtype MerkleLink = MerkleLink Cid -- MultihashDigest
  deriving (Eq, Show, Generic)

instance Serialise MerkleLink where
  encode (MerkleLink loc) = encodeTag cborTagLink <> encode loc
  decode = decodeTag *> decode

instance Serialise Value where
  encode = \case
    LinkValue link -> encode link
    TextValue text -> encode text
    DagObject hmap -> encode hmap
    DagArray  arr  -> encode arr

  decode = do
    tkty <- peekTokenType
    case tkty of
      TypeTag -> do
        _tag <- decodeTag
        LinkValue <$> decode
      TypeString -> TextValue <$> decode
      TypeListLen -> DagArray <$> decode
      TypeMapLen -> DagObject <$> decode
      _ -> fail $ "unexpected CBOR token type for IPLD value: " ++ show tkty

data Hierarchy = Ipfs

data Row
  = LinkRow MerkleLink
  | ValueRow Text Value
  deriving (Eq, Show, Generic)

data Value
  = LinkValue MerkleLink
  | TextValue Text -- TODO should this be a bytestring?
  | DagObject (HashMap Text Value)
  | DagArray (Vector Value)
  -- Should there also be number, bool, null?
  deriving (Eq, Show, Generic)

instance IsString Value where
  fromString = TextValue . fromString

-- | A merkle-path consisting of
-- * merkle-link
-- * path traversal
data MerklePath = MerklePath MerkleLink [Text]
  deriving Show

data TraversalResult a
  = Found a
  -- | Yield (a, MerklePath)

  -- "cross-object traversal"
  | Yield MerkleLink [Text]
  | KeyNotFound a [Text]
  deriving (Eq, Show)

traverseValue :: [Text] -> Value -> TraversalResult Value
traverseValue [] v = Found v
traverseValue path@(a:as) val@(DagArray vals) =
  let mayVal = do
        i <- readMaybe (T.unpack a)
        vals ^? ix i
  in case mayVal of
       Just val -> traverseValue as val
       Nothing -> KeyNotFound val path
traverseValue path@(a:as) val@(DagObject vals) = case vals ^? ix a of
   Just val -> traverseValue as val
   Nothing -> KeyNotFound val path
traverseValue path val@(TextValue _) = KeyNotFound val path
traverseValue path (LinkValue link) = Yield link path


(.=) :: Text -> Value -> Row
(.=) = ValueRow

infixr 8 .=

object :: [Row] -> Value
object = DagObject . HashMap.fromList . map makeTuple where
  makeTuple :: Row -> (Text, Value)
  makeTuple (LinkRow l) = ("/", LinkValue l) -- TODO this line is wrong
  makeTuple (ValueRow k v) = (k, v)

array :: [Value] -> Value
array = DagArray . V.fromList

merkleLink :: Text -> Maybe MerkleLink
merkleLink text = case parseOnly (parseMerkleLink <* endOfInput) text of
  Left _     -> Nothing
  Right link -> Just link

merkleLink' :: Text -> Maybe Value
merkleLink' text = LinkValue <$> merkleLink text

-- $parsing

-- eg:
--   "/ipfs/QmUmg7BZC1YP1ca66rRtWKxpXp77WgVHrnv263JtDuvs2k/a/b/c"
--   ->
--   MerklePath ["QmUmg7BZC1YP1ca66rRtWKxpXp77WgVHrnv263JtDuvs2k", "a", "b", "c"]
--
merklePath :: Text -> Either String MerklePath
merklePath = parseOnly (parseMerklePath <* endOfInput)

parseMerklePath :: Parser MerklePath
parseMerklePath = do
  link <- parseMerkleLink
  traversal <- (do
    _ <- char '/'
    takeWhile (not . (== '/')) `sepBy` (char '/')
    ) <|> pure []
  pure (MerklePath link traversal)

-- "currently, only the ipfs hierarchy is allowed"
-- TODO: allow option for "/ipfs" or not
parseMerkleLink :: Parser MerkleLink
parseMerkleLink = MerkleLink <$> parseIpfsMultiaddr

-- TODO: for now we don't parse multihashes in full generality -- we only allow
-- one protocol / location pair. In fact, we only allow /ipfs/. And we don't
-- try to hard on verifying the identifier is okay.
parseIpfsMultiaddr :: Parser Cid
parseIpfsMultiaddr = do
  proto <- string "/ipfs"
  _ <- char '/'
  ident <- takeWhile (not . (== '/'))
  undefined "TODO: parseMultiaddr"
  -- case readMultiaddr (proto <> "/" <> ident) of
  --   Just x -> pure x
  --   Nothing -> empty

base58Class :: String
base58Class = B8.unpack $ unAlphabet bitcoinAlphabet

parseCid :: ABS.Parser Cid
parseCid = do
  base <- ABS.anyWord8
  case toEnum $ w8ToI base of
    Base58Btc -> do
      b58str <- BS.pack <$> ABS.count 48 (ABS.satisfy (ABS.inClass base58Class))
      case decodeBase58 bitcoinAlphabet b58str of
        Nothing -> fail "invalid base-58 string"
        Just byteStr -> do
          let version:codec:hashVal = BS.unpack byteStr
          -- traceShowM $ encodeBase58 bitcoinAlphabet $ BS.pack hashVal
          case (version, codec) of
            (1, 0x71) -> pure $ mkCid byteStr
            _ -> fail $ "Can't currently parse other than cid v1, dag-cbor"
    _ -> fail "Can't currently parse other than base58btc"

-- $class

class IsIpld a where
  toIpld :: a -> Value

  -- TODO: more expressive failure info
  fromIpld :: Value -> Maybe a

instance IsIpld Value where
  toIpld = id
  fromIpld = Just
