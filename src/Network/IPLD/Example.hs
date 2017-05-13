{-# language OverloadedStrings #-}
module Network.IPLD.Example () where

import Data.Maybe (fromJust)

import Network.IPLD.Internal
-- import Network.Multiaddr

linkExample :: MerkleLink
linkExample = fromJust $ merkleLink
  "/ipfs/QmUmg7BZC1YP1ca66rRtWKxpXp77WgVHrnv263JtDuvs2k"
  -- "QmUmg7BZC1YP1ca66rRtWKxpXp77WgVHrnv263JtDuvs2k"

helloWorldExample :: Value
helloWorldExample = TextValue "hello, world"

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

pathExample1 :: AbsMerklePath
pathExample1 = AbsMerklePath
  (fromJust $ merkleLink "/ipfs/QmUmg7BZC1YP1ca66rRtWKxpXp77WgVHrnv263JtDuvs2k")
  (RelMerklePath ["a", "b", "c"])

pathExample2, pathExample3, pathExample4, pathExample5
  :: Either String AbsMerklePath
pathExample2 = absMerklePath
  "/ipfs/QmUmg7BZC1YP1ca66rRtWKxpXp77WgVHrnv263JtDuvs2k/a/b/link/c"
pathExample3 = absMerklePath
  "/ipfs/QmUmg7BZC1YP1ca66rRtWKxpXp77WgVHrnv263JtDuvs2k/a/b/link/d/e"
pathExample4 = absMerklePath
  "/ipfs/QmUmg7BZC1YP1ca66rRtWKxpXp77WgVHrnv263JtDuvs2k/a/b/link/foo/name"
pathExample5 = absMerklePath
  "/ipfs/QmUmg7BZC1YP1ca66rRtWKxpXp77WgVHrnv263JtDuvs2k/a/b/foo/name"
