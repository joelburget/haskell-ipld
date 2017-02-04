{-# language BangPatterns #-}
{-# language OverloadedStrings #-}

module Network.IPLD.Internal where

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.String
import Data.Text (Text)
import Data.Vector (Vector)
import qualified Data.Vector as V

data Hierarchy = Ipfs

-- TODO: user multihash (or cryptonite?) here
-- TODO: Should we require /ipfs/ prefix?
newtype MerkleLink = MerkleLink Text -- MultihashDigest

-- TODO: check validity
instance IsString MerkleLink where
  fromString = MerkleLink . fromString

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

merkleLink :: Text -> Value
merkleLink = LinkValue . MerkleLink

-- examples:

linkExample :: Value
linkExample = merkleLink "/ipfs/QmUmg7BZC1YP1ca66rRtWKxpXp77WgVHrnv263JtDuvs2k"

fooBazExample :: Value
fooBazExample = object
  [ "bar" .= "/ipfs/QmUmg7BZC1YP1ca66rRtWKxpXp77WgVHrnv263JtDuvs2k" -- not a link
  , "baz" .= merkleLink "/ipfs/QmUmg7BZC1YP1ca66rRtWKxpXp77WgVHrnv263JtDuvs2k" -- link
  ]

catPhotoExample :: Value
catPhotoExample = object
  [ "files" .= object
    [ "cat.jpg" .= object
      [ LinkRow (MerkleLink "/ipfs/QmUmg7BZC1YP1ca66rRtWKxpXp77WgVHrnv263JtDuvs2k")
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
        "link" .= merkleLink "QmAAA...",
        "size" .= "100324"
      ]
    , object
      [
        "link" .= merkleLink "QmAA1...",
        "size" .= "120345",
        "repeat" .= "10"
      ]
    , object
      [
        "link" .= merkleLink "QmAA1...",
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

pathExample1 = MerklePath "/ipfs/QmUmg7BZC1YP1ca66rRtWKxpXp77WgVHrnv263JtDuvs2k" ["a", "b", "c"]
pathExample2 = merklePath "/ipfs/QmUmg7BZC1YP1ca66rRtWKxpXp77WgVHrnv263JtDuvs2k/a/b/link/c"
pathExample3 = merklePath "/ipfs/QmUmg7BZC1YP1ca66rRtWKxpXp77WgVHrnv263JtDuvs2k/a/b/link/d/e"
pathExample4 = merklePath "/ipfs/QmUmg7BZC1YP1ca66rRtWKxpXp77WgVHrnv263JtDuvs2k/a/b/link/foo/name"
pathExample5 = merklePath "/ipfs/QmUmg7BZC1YP1ca66rRtWKxpXp77WgVHrnv263JtDuvs2k/a/b/foo/name"
