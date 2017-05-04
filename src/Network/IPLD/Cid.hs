{-# language OverloadedStrings #-}
{-# language LambdaCase #-}
{-# language TypeApplications #-}
module Network.IPLD.Cid where

import Crypto.Hash
import Data.Monoid ((<>))
import Data.Byteable (toBytes)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import Data.Text (Text)
import Data.Text.Lazy (toStrict)
import Data.Text.Format
import Data.Text.Encoding (decodeUtf8)
import Data.Word (Word8)
import Data.ByteString.Base58

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
  deriving (Eq, Show)

class HumanOrCompact a where
  human :: a -> Text
  compact :: a -> ByteString

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
    Identity_Multibase -> error "TODO"
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
  deriving (Eq, Show)

instance Enum CodecId where
  toEnum = \case
    0x70 -> DagCbor

  fromEnum = \case
    DagCbor -> 0x70

instance HumanOrCompact CodecId where
  human DagCbor = "dag-cbor"
  compact DagCbor = BS.pack [0x70]

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
  deriving (Eq, Show)

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

  compact = BS.pack . pure . fromInteger . toInteger . fromEnum

data Multihash = Multihash
  HashFunction   -- hash function
  UnsignedVarint -- digest size (in bytes)
  ByteString     -- hash function output
  deriving (Eq, Show)

instance HumanOrCompact Multihash where
  compact (Multihash fun size bs) =
    let bs' = encodeBase58 bitcoinAlphabet bs
    in compact fun <> B8.pack (show size) <> bs'

  human (Multihash fun size bs) =
    let bs' = decodeUtf8 $ encodeBase58 bitcoinAlphabet bs
    in toStrict $ format "{}-{}-{}"
         ( human fun
         , show size
         -- TODO: hardcoded to base 58...
         , bs'
         )

data Cid = Cid
  Multibase
  UnsignedVarint
  -- Multicodec
  CodecId
  Multihash
  deriving (Eq, Show)

-- Qm: sha2-256, 32 bytes, base-58 encoding
hashOf :: ByteString -> Multihash
hashOf = Multihash Sha2_256 32 . toBytes . hash @SHA256

mkCid :: ByteString -> Cid
mkCid = Cid Base58Btc 1 DagCbor . hashOf

-- instance Serialize Cid where

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

  compact (Cid base version codec multihash)
    = compact base
   <> B8.pack (show version)
   <> compact codec
   <> compact multihash
