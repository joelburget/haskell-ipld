{-# language OverloadedStrings #-}
{-# language DeriveGeneric #-}
{-# language LambdaCase #-}
{-# language DeriveAnyClass #-}
{-# language DeriveDataTypeable #-}

module Network.IPLD.Cid where

import           Crypto.Hash
import           Data.ByteString (ByteString)
import           Data.ByteString.Base58
import           Data.Data
import           Data.Hashable (Hashable)
import           Data.Monoid ((<>))
import           Data.Text (Text)
import           Data.Text.Encoding (decodeUtf8)
import           Data.Text.Format
import           Data.Text.Lazy (toStrict)
import           Data.Word (Word8)
import           GHC.Generics
import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as Hex

import           Data.Binary.Serialise.CBOR.Class

-- pattern Identity_MultibaseI :: Int
-- pattern Identity_MultibaseI = 0

data Multibase
  = Identity_Multibase
  | Base1
  | Base2
  | Base8
  | Base10
  | Base16
  | Base32
  | Base32Pad
  | Base32Hex
  | Base32HexPad
  | Base32z
  | Base58Flickr
  | Base58Btc
  | Base64
  | Base64Pad
  | Base64Url
  | Base64UrlPad
  deriving (Eq, Show, Generic, Hashable, Typeable, Data)

class HumanOrCompact a where
  human :: a -> Text
  compact :: a -> ByteString

instance Enum Multibase where
  toEnum = \case
    0x00 -> Identity_Multibase
    other -> case toEnum other of
      '1' -> Base1
      '0' -> Base2
      '7' -> Base8
      '9' -> Base10
      'f' -> Base16
      'b' -> Base32
      'c' -> Base32Pad
      'v' -> Base32Hex
      't' -> Base32HexPad
      'h' -> Base32z
      'Z' -> Base58Flickr
      'z' -> Base58Btc
      'm' -> Base64
      'u' -> Base64Url
      'M' -> Base64Pad
      'U' -> Base64UrlPad
      _ -> error "Enum Multibase"

      -- 'F' -> Base16Upper
      -- 'B' -> Base32Upper
      -- 'C' -> Base32padUpper
      -- 'V' -> Base32hexUpper
      -- 'T' -> Base32hexPadUpper

  fromEnum = \case
    Identity_Multibase -> 0x00
    other -> fromEnum $ case other of
      Base1              -> '1'
      Base2              -> '0'
      Base8              -> '7'
      Base10             -> '9'
      Base16             -> 'f'
      Base32             -> 'b'
      Base32Pad          -> 'c'
      Base32Hex          -> 'v'
      Base32HexPad       -> 't'
      Base32z            -> 'h'
      Base58Flickr       -> 'Z'
      Base58Btc          -> 'z'
      Base64             -> 'm'
      Base64Url          -> 'u'
      Base64Pad          -> 'M'
      Base64UrlPad       -> 'U'
      Identity_Multibase -> undefined

instance HumanOrCompact Multibase where
  human = \case
    Identity_Multibase -> "identity"
    Base1              -> "base1"
    Base2              -> "base2"
    Base8              -> "base8"
    Base10             -> "base10"
    Base16             -> "base16"
    Base32             -> "base32"
    Base32Pad          -> "base32pad"
    Base32Hex          -> "base32hex"
    Base32HexPad       -> "base32hexpad"
    Base32z            -> "base32z"
    Base58Flickr       -> "base58flickr"
    Base58Btc          -> "base58btc"
    Base64             -> "base64"
    Base64Pad          -> "base64pad"
    Base64Url          -> "base64url"
    Base64UrlPad       -> "base64urlpad"

  compact = \case
    Identity_Multibase -> BS.pack [0]
    Base1              -> "1"
    Base2              -> "0"
    Base8              -> "7"
    Base10             -> "9"
    Base16             -> "f"
    Base32             -> "b"
    Base32Pad          -> "c"
    Base32Hex          -> "v"
    Base32HexPad       -> "t"
    Base32z            -> "h"
    Base58Flickr       -> "Z"
    Base58Btc          -> "z"
    Base64             -> "m"
    Base64Pad          -> "M"
    Base64Url          -> "u"
    Base64UrlPad       -> "U"

-- TODO: actually implement varint
-- (this is fine as long as we only use the low 7 bits)
type UnsignedVarint = Word8

data CodecId
  = DagCbor
  deriving (Eq, Show, Generic, Hashable, Typeable, Data)

instance Enum CodecId where
  toEnum = \case
    0x71 -> DagCbor
    _ -> error "toEnum CodecId"

  fromEnum = \case
    DagCbor -> 0x71

instance HumanOrCompact CodecId where
  human DagCbor = "dag-cbor"
  compact DagCbor = BS.pack [0x71]

data Multicodec = Multicodec
  CodecId
  ByteString

data HashFunction
  = Identity_HashFunction
  | Sha1
  | Sha2_256
  | Sha2_512
  | Sha3_224
  | Sha3_256
  | Sha3_384
  | Sha3_512
  deriving (Eq, Show, Generic, Hashable, Typeable, Data)

instance Enum HashFunction where
  toEnum = \case
    0x00 -> Identity_HashFunction
    0x11 -> Sha1
    0x12 -> Sha2_256
    0x13 -> Sha2_512
    0x17 -> Sha3_224
    0x16 -> Sha3_256
    0x15 -> Sha3_384
    0x14 -> Sha3_512
    _    -> error "bad HashFunction enum"

  fromEnum = \case
    Identity_HashFunction -> 0x00
    Sha1                  -> 0x11
    Sha2_256              -> 0x12
    Sha2_512              -> 0x13
    Sha3_224              -> 0x17
    Sha3_256              -> 0x16
    Sha3_384              -> 0x15
    Sha3_512              -> 0x14

instance HumanOrCompact HashFunction where
  human = \case
    Identity_HashFunction -> "identity"
    Sha1                  -> "sha1"
    Sha2_256              -> "sha2-256"
    Sha2_512              -> "sha2-512"
    Sha3_224              -> "sha3-224"
    Sha3_256              -> "sha3-256"
    Sha3_384              -> "sha3-384"
    Sha3_512              -> "sha3-512"

  compact = BS.pack . pure . iToW8 . fromEnum

data Multihash = Multihash
  HashFunction   -- hash function
  UnsignedVarint -- digest size (in bytes)
  ByteString     -- hash function output
  deriving (Eq, Generic, Hashable, Typeable, Data)

instance Show Multihash where
  showsPrec d (Multihash fun size bs) = showParen (d > 10) $
      showString "Multihash "
    . showsPrec 11 fun
    . showString " "
    . showsPrec 11 size
    . showString " "
    . showsPrec 11 (Hex.encode bs)

-- both of these can be done with lens? Numeric.Lens.integral
iToW8 :: Int -> Word8
iToW8 = fromInteger . toInteger

w8ToI :: Word8 -> Int
w8ToI = fromInteger . toInteger

instance HumanOrCompact Multihash where
  compact (Multihash fun size bs) = compact fun <> BS.pack [size] <> bs

  human (Multihash fun size bs) =
    let bs' = decodeUtf8 $ encodeBase58 bitcoinAlphabet bs
    in toStrict $ format "{}-{}-{}"
         ( human fun
         , show size
         -- TODO: hardcoded to base 58...
         , bs'
         )

instance Serialise Multihash where
  encode = encode . compact
  decode = undefined

data Cid = Cid
  Multibase
  UnsignedVarint
  -- Multicodec
  CodecId
  Multihash
  deriving (Eq, Show, Generic, Hashable, Typeable, Data)

-- TODO: use http://hackage.haskell.org/package/concise-0.1.0.0/docs/Control-Lens-Cons-Extras.html ?
toByteString :: BA.ByteArrayAccess a => a -> ByteString
toByteString = BS.pack . BA.unpack

-- Qm: sha2-256, 32 bytes, base-58 encoding
hashOf :: ByteString -> Multihash
hashOf bs =
  let digest = hash bs :: Digest SHA256
  in Multihash Sha2_256 32 $ toByteString digest

mkCid :: ByteString -> Cid
mkCid = Cid Base58Btc 1 DagCbor . hashOf

instance HumanOrCompact Cid where
  human (Cid base version codec multihash) =
    let versionStr = case version of
          1 -> "cidv1"
          2 -> "cidv2"
          _ -> error "bad CID version"
        codecStr = case codec of
          DagCbor -> "dag-cbor"
    in toStrict $ format "{}-{}-{}-{}"
         ( human base
         , versionStr :: Text
         , codecStr :: Text
         , human multihash
         )

  compact (Cid base version codec multihash) =
    let base' = compact base
        rest = BS.pack [version] <> compact codec <> compact multihash
    in base' <> encodeBase58 bitcoinAlphabet rest

instance Serialise Cid where
  encode = encode . compact
  decode = undefined "TODO: decode Cid"
