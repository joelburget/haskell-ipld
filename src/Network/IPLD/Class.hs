module Network.IPLD.Class where

class IsIpld a where
  toIpld :: a -> Value

  -- TODO: more expressive failure info
  fromIpld :: Value -> Maybe Ipld
