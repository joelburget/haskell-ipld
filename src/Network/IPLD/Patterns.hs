{-# language PatternSynonyms #-}
{-# language ViewPatterns #-}
module Network.IPLD.Patterns
  ( pattern Vx
  , pattern T1
  , pattern T2
  , pattern T3
  , pattern T4
  , pattern T5
  ) where

import           Data.Text (Text)
import qualified Data.Vector as V

import Network.IPLD.Class
import Network.IPLD.Internal

pattern Vx :: [a] -> V.Vector a
pattern Vx lst <- (V.toList -> lst) where
  Vx lst = V.fromList lst

pattern T1 :: IsIpld a => Text -> a -> Value
pattern T1 tag a <- (DagArray (Vx [fromIpld -> Just tag, fromIpld -> Just a])) where
  T1 tag a = DagArray (Vx [toIpld tag, toIpld a])

pattern T2 :: (IsIpld a, IsIpld b) => Text -> a -> b -> Value
pattern T2 tag a b <- (DagArray (Vx [fromIpld -> Just tag, fromIpld -> Just a, fromIpld -> Just b])) where
  T2 tag a b = DagArray (Vx [toIpld tag, toIpld a, toIpld b])

pattern T3 :: (IsIpld a, IsIpld b, IsIpld c) => Text -> a -> b -> c -> Value
pattern T3 tag a b c <- (DagArray (Vx [fromIpld -> Just tag, fromIpld -> Just a, fromIpld -> Just b, fromIpld -> Just c])) where
  T3 tag a b c = DagArray (Vx [toIpld tag, toIpld a, toIpld b, toIpld c])

pattern T4 :: (IsIpld a, IsIpld b, IsIpld c, IsIpld d) => Text -> a -> b -> c -> d -> Value
pattern T4 tag a b c d <- (DagArray (Vx [fromIpld -> Just tag, fromIpld -> Just a, fromIpld -> Just b, fromIpld -> Just c, fromIpld -> Just d])) where
  T4 tag a b c d = DagArray (Vx [toIpld tag, toIpld a, toIpld b, toIpld c, toIpld d])

pattern T5 :: (IsIpld a, IsIpld b, IsIpld c, IsIpld d, IsIpld e) => Text -> a -> b -> c -> d -> e -> Value
pattern T5 tag a b c d e <- (DagArray (Vx [fromIpld -> Just tag, fromIpld -> Just a, fromIpld -> Just b, fromIpld -> Just c, fromIpld -> Just d, fromIpld -> Just e])) where
  T5 tag a b c d e = DagArray (Vx [toIpld tag, toIpld a, toIpld b, toIpld c, toIpld d, toIpld e])
