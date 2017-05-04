{-# language BangPatterns #-}
{-# language OverloadedStrings #-}

module Network.IPLD.Internal where

import Prelude hiding (takeWhile)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Maybe (fromJust)
import Data.String
import Data.Text (Text)
import Data.Vector (Vector)
import qualified Data.Vector as V
import Network.Multiaddr

import Data.Attoparsec.Text

import qualified Data.Binary.Serialise.CBOR.Read     as CBOR.Read
import qualified Data.Binary.Serialise.CBOR.Write    as CBOR.Write
import           Data.Binary.Serialise.CBOR.Encoding
import           Data.Binary.Serialise.CBOR.Decoding

import           Data.Binary.Serialise.CBOR.Class

cborTagLink :: Word
cborTagLink = 42

-- instance Serialise MerkleLink where
--   -- encode :: MerkleLink -> Encoding
--   encode (MerkleLink loc) = encodeTag cborTagLink -- <>

-- instance Serialise Row where
-- instance Serialise Value where

data Hierarchy = Ipfs

-- TODO: user multihash (or cryptonite?) here
-- TODO: Should we require /ipfs/ prefix?
newtype MerkleLink = MerkleLink Multiaddr -- MultihashDigest

-- TODO: check validity
-- instance IsString MerkleLink where
--   fromString = MerkleLink . fromString

data Row
  = LinkRow MerkleLink
  | ValueRow Text Value

data Value
  = LinkValue MerkleLink
  | TextValue Text
  | DagObject (HashMap Text Value)
  | DagArray (Vector Value)

instance IsString Value where
  fromString = TextValue . fromString

-- | A merkle-path consisting of
-- * merkle-link
-- * path traversal
data MerklePath = MerklePath MerkleLink [Text]

data TraversalResult
  = Found Value
  | Yield MerklePath
  | KeyNotFound MerklePath

traversePath :: MerklePath -> Value -> TraversalResult
traversePath (MerklePath link []) v = Found v
traversePath (MerklePath link (a:as)) v = undefined

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

merkleLink :: Text -> Maybe Value
merkleLink text = LinkValue . MerkleLink <$> readMultiaddr text

-- examples:

linkExample :: MerkleLink
linkExample = fromJust $ MerkleLink <$> readMultiaddr "/ipfs/QmUmg7BZC1YP1ca66rRtWKxpXp77WgVHrnv263JtDuvs2k"

fooBazExample :: Value
fooBazExample = object
  [ "bar" .= "/ipfs/QmUmg7BZC1YP1ca66rRtWKxpXp77WgVHrnv263JtDuvs2k" -- not a link
  , "baz" .= LinkValue linkExample -- link
  ]

catPhotoExample :: Value
catPhotoExample = object
  [ "files" .= object
    [ "cat.jpg" .= object
      [ LinkRow linkExample
      , "mode" .= "0755"
      , "owner" .= "jbenet"
      ]
    ]
  ]

chunkedFileExample :: Value
chunkedFileExample = object
  [ "size" .= "1424119"
  , "subfiles" .= array
    [ object
      [
        -- "link" .= merkleLink "QmAAA...",
        "size" .= "100324"
      ]
    , object
      [
        -- "link" .= merkleLink "QmAA1...",
        "size" .= "120345",
        "repeat" .= "10"
      ]
    , object
      [
        -- "link" .= merkleLink "QmAA1...",
        "size" .= "120345"
      ]
    ]
  ]

-- eg:
--   "/ipfs/QmUmg7BZC1YP1ca66rRtWKxpXp77WgVHrnv263JtDuvs2k/a/b/c"
--   ->
--   MerklePath ["QmUmg7BZC1YP1ca66rRtWKxpXp77WgVHrnv263JtDuvs2k", "a", "b", "c"]
--
-- TODO: probably use attoparsec
merklePath :: Text -> Maybe MerklePath
merklePath = undefined

-- pathExample1 = MerklePath "/ipfs/QmUmg7BZC1YP1ca66rRtWKxpXp77WgVHrnv263JtDuvs2k" ["a", "b", "c"]
pathExample2 = merklePath "/ipfs/QmUmg7BZC1YP1ca66rRtWKxpXp77WgVHrnv263JtDuvs2k/a/b/link/c"
pathExample3 = merklePath "/ipfs/QmUmg7BZC1YP1ca66rRtWKxpXp77WgVHrnv263JtDuvs2k/a/b/link/d/e"
pathExample4 = merklePath "/ipfs/QmUmg7BZC1YP1ca66rRtWKxpXp77WgVHrnv263JtDuvs2k/a/b/link/foo/name"
pathExample5 = merklePath "/ipfs/QmUmg7BZC1YP1ca66rRtWKxpXp77WgVHrnv263JtDuvs2k/a/b/foo/name"

-- $parsing

parseMerklePath :: Parser MerklePath
parseMerklePath = do
  link <- parseMerkleLink
  traversal <- do
    _ <- try $ char '/'
    takeWhile (not . (==) '/') `sepBy` (char '/')
  pure (MerklePath link traversal)

parseMerkleLink :: Parser MerkleLink
parseMerkleLink = do
  protocol <- parseProtocolNamespace
  hash <- parseMultihash
  pure (MerkleLink protocol hash)

parseProtocolNamespace :: Parser Text
parseProtocolNamespace = _

parseMultihash :: Parser Text
parseMultihash = _
