{-# language LambdaCase #-}
{-# language Rank2Types #-}
{-# language FlexibleContexts #-}
module Network.IPLD.Lens where

-- Most code in this module taken from:
-- https://hackage.haskell.org/package/lens-aeson-1.0.1/docs/src/Data-Aeson-Lens.html

import Control.Lens
import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
import Data.Vector (Vector)

import Network.IPLD.Internal

_DagObject :: Prism' Value (HashMap Text Value)
_DagObject = prism DagObject (\case DagObject o -> Right o; v -> Left v)

_DagArray :: Prism' Value (Vector Value)
_DagArray = prism DagArray (\case DagArray a -> Right a; v -> Left v)

-- |
-- Like 'ix', but for 'DagObject' with Text indices. This often has better
-- inference than 'ix' when used with OverloadedStrings.
--
-- >>> "{\"a\": 100, \"b\": 200}" ^? key "a"
-- Just (Number 100.0)
--
-- >>> "[1,2,3]" ^? key "a"
-- Nothing
key :: Text -> Traversal' Value Value
key i = _DagObject . ix i
{-# INLINE key #-}

-- | An indexed Traversal into DagObject properties
--
-- >>> "{\"a\": 4, \"b\": 7}" ^@.. members
-- [("a",Number 4.0),("b",Number 7.0)]
--
-- >>> "{\"a\": 4, \"b\": 7}" & members . _Number *~ 10
-- "{\"a\":40,\"b\":70}"
members :: IndexedTraversal' Text Value Value
members = _DagObject . itraversed
{-# INLINE members #-}

-- | Like 'ix', but for DagArrays
--
-- >>> "[1,2,3]" ^? nth 1
-- Just (Number 2.0)
--
-- >>> "\"a\": 100, \"b\": 200}" ^? nth 1
-- Nothing
--
-- >>> "[1,2,3]" & nth 1 .~ Number 20
-- "[1,20,3]"
nth :: Int -> Traversal' Value Value
nth i = _DagArray . ix i
{-# INLINE nth #-}

-- | An indexed Traversal into DagArray elements
--
-- >>> "[1,2,3]" ^.. values
-- [Number 1.0,Number 2.0,Number 3.0]
--
-- >>> "[1,2,3]" & values . _Number *~ 10
-- "[10,20,30]"
values :: IndexedTraversal' Int Value Value
values = _DagArray . traversed
{-# INLINE values #-}
