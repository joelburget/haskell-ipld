{-# language BangPatterns #-}
{-# language DeriveGeneric #-}
{-# language LambdaCase #-}
{-# language OverloadedStrings #-}

module Network.IPLD.Internal where

import Prelude hiding (takeWhile)

import Control.Applicative
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Monoid ((<>))
import Data.String
import Data.Text (Text)
import Data.Vector (Vector)
import qualified Data.Vector as V
import GHC.Generics
import Network.Multiaddr hiding (encode, decode)

import Data.Attoparsec.Text

import           Data.Binary.Serialise.CBOR.Class
import           Data.Binary.Serialise.CBOR.Encoding
import           Data.Binary.Serialise.CBOR.Decoding

-- $coding
--
-- The key here is adhering to the cbor standard specified in the ipld spec

cborTagLink :: Word
cborTagLink = 42

instance Serialise MerkleLink where
  encode (MerkleLink loc) = encodeTag cborTagLink <> encodeAddr loc
    where encodeAddr addr = case parts addr of
            [IPFS addr'] -> encodeBytes addr'
            _ -> error $ "invalid link addr: " ++ show addr

  decode = decodeTag *> decodeAddr
    where decodeAddr = undefined

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

newtype MerkleLink = MerkleLink Multiaddr -- MultihashDigest
  deriving (Eq, Show, Generic)

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

data TraversalResult
  = Found Value
  | Yield MerklePath
  | KeyNotFound MerklePath

traversePath :: MerklePath -> Value -> TraversalResult
traversePath (MerklePath _link []) v = Found v
traversePath path@(MerklePath link (a:as)) (DagArray vals) =
  undefined
  -- let foo = do
  --       i <- readMay a
  --       vals ^? ix i
  -- in case foo of
  --      Just val -> Found _
  --      Nothing -> KeyNotFound path


(.=) :: Text -> Value -> Row
(.=) = ValueRow

infixr 8 .=

object :: [Row] -> Value
object = DagObject . HashMap.fromList . map makeTuple where
  makeTuple :: Row -> (Text, Value)
  makeTuple (LinkRow l) = ("/", LinkValue l)
  makeTuple (ValueRow k v) = (k, v)

array :: [Value] -> Value
array = DagArray . V.fromList

merkleLink :: Text -> Maybe MerkleLink
merkleLink text = MerkleLink <$> readMultiaddr text

merkleLink' :: Text -> Maybe Value
merkleLink' text = LinkValue <$> merkleLink text

-- $parsing

-- eg:
--   "/ipfs/QmUmg7BZC1YP1ca66rRtWKxpXp77WgVHrnv263JtDuvs2k/a/b/c"
--   ->
--   MerklePath ["QmUmg7BZC1YP1ca66rRtWKxpXp77WgVHrnv263JtDuvs2k", "a", "b", "c"]
--
merklePath :: Text -> Either String MerklePath
merklePath = parseOnly parseMerklePath

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
parseMerkleLink = MerkleLink <$> parseMultiaddr

-- TODO: for now we don't parse multihashes in full generality -- we only allow
-- one protocol / location pair. In fact, we only allow /ipfs/. And we don't
-- try to hard on verifying the identifier is okay.
parseMultiaddr :: Parser Multiaddr
parseMultiaddr = do
  proto <- string "/ipfs"
  _ <- char '/'
  ident <- takeWhile (not . (== '/'))
  case readMultiaddr (proto <> "/" <> ident) of
    Just x -> pure x
    Nothing -> empty

-- $class

class IsIpld a where
  toIpld :: a -> Value

  -- TODO: more expressive failure info
  fromIpld :: Value -> Maybe a

instance IsIpld Value where
  toIpld = id
  fromIpld = Just
