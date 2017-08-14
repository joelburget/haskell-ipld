{-# language BangPatterns #-}
{-# language CPP #-}
{-# language DataKinds #-}
{-# language DeriveDataTypeable #-}
{-# language DeriveGeneric #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language LambdaCase #-}
{-# language OverloadedStrings #-}
{-# language ScopedTypeVariables #-}
{-# language TypeFamilies #-}
{-# language TypeOperators #-}
{-# language TypeSynonymInstances #-}
{-# options_ghc -funbox-strict-fields #-}

module Network.IPLD.Internal
  ( MerkleLink(..)
  , AbsMerklePath(..)
  , RelMerklePath(..)
  , Value(..)
  , Row(..)
  , TraversalResult(..)
  , absMerklePath
  , (.=)
  , object
  , array
  , valueCid
  , merkleLink
  , traverseValue
  , readCid
  , parseCid -- TODO: move to Cid module
  , toAeson
  , fromAeson
  , jsonEncode
  , jsonDecode
  , graft
  , graftM
  , link
  , linkM
  , linkToM
  , linkToV

  -- encoding  / decoding
  , encodeValue
  , decodeValue

  , (<$$>)
  ) where

import Prelude hiding (takeWhile)

import           Control.Applicative
import           Control.Lens
  ((^?), Index, IxValue, Ixed(..), transformM, Plated, rewriteM)
import           Control.Monad (when)
import qualified Data.Aeson as Aeson
import           Data.ByteString.Base58
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString as SBS
import           Data.Data
import           Data.Functor.Identity
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import           Data.Hashable
import           Data.Maybe (fromMaybe)
import           Data.Monoid ((<>))
import           Data.Scientific
import           Data.String
import           Data.Text (Text)
import           Data.Text.Encoding (decodeUtf8, encodeUtf8)
import           Data.Vector (Vector)
import           Data.Word
import           GHC.Generics
import           Text.Read hiding (get)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as Hex
import qualified Data.ByteString.Char8 as B8
import qualified Data.Text as T
import qualified Data.Vector as V

import           Data.Attoparsec.Text
import qualified Data.Attoparsec.ByteString as ABS

-- import           Data.Binary.Serialise.CBOR
import           Codec.CBOR.Decoding
import           Codec.CBOR.Encoding
import           Codec.CBOR.Write

import Network.IPLD.Cid

-- $coding
--
-- The key here is adhering to the cbor standard specified in the ipld spec

cborTagLink :: Word
cborTagLink = 42

-- TODO: This is so similar to Data.Aeson.Value, just with the addition of
-- links -- is there any way to not duplicate all their work?
data Value
  = LinkValue !MerkleLink
  | DagObject !(HashMap Text Value)
  | DagArray  !(Vector Value)
  | TextValue !Text
  | DagNumber !Scientific
  | DagBool   !Bool
  | Null
  deriving (Eq, Show, Generic, Typeable, Data)

toAeson :: Value -> Aeson.Value
toAeson = \case
  LinkValue (MerkleLink cid) -> Aeson.object
    [ "/" Aeson..= decodeUtf8 (compact cid) ]
  DagObject hmap -> Aeson.Object (toAeson <$> hmap)
  DagArray  arr  -> Aeson.Array  (toAeson <$> arr)
  TextValue text -> Aeson.String text
  DagNumber num  -> Aeson.Number num
  DagBool   bool -> Aeson.Bool   bool
  Null           -> Aeson.Null

fromAeson :: Aeson.Value -> Value
fromAeson = \case
  Aeson.Object hmap ->
    let defaultObj = DagObject (fromAeson <$> hmap)
    in fromMaybe defaultObj $ do
         ["/"]                       <- pure $ HashMap.keys hmap
         Just (Aeson.String linkStr) <- pure $ hmap ^? ix "/"
         Right cid                   <- pure $
           ABS.parseOnly parseCid (encodeUtf8 linkStr)
         pure $ LinkValue (MerkleLink cid)
  Aeson.Array  arr  -> DagArray  (fromAeson <$> arr)
  Aeson.String text               -> TextValue text
  Aeson.Number num                -> DagNumber num
  Aeson.Bool   bool               -> DagBool   bool
  Aeson.Null                      -> Null

infix 8 <$$>
(<$$>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<$$>) = fmap . fmap

-- | JSON-encode a value.
jsonEncode :: Value -> SBS.ByteString
jsonEncode = LBS.toStrict . Aeson.encode . toAeson

-- | JSON-decode a value.
jsonDecode :: SBS.ByteString -> Maybe Value
jsonDecode = fromAeson <$$> Aeson.decode' . LBS.fromStrict

newtype MerkleLink = MerkleLink Cid -- MultihashDigest
  deriving (Eq, Show, Generic, Hashable, Typeable, Data)

encodeMerkleLink :: MerkleLink -> Encoding
encodeMerkleLink (MerkleLink loc) = encodeTag cborTagLink <> encodeCid loc

decodeMerkleLink :: Decoder s MerkleLink
decodeMerkleLink = MerkleLink <$> (decodeTag *> decodeCid)

-- decodeListN, decodeListIndefLen, decodeMapN taken from Codec.CBOR.Term
decodeListN :: Int -> [Value] -> Decoder s Value
decodeListN !n acc =
  case n of
    0 -> return $! DagArray (V.reverse (V.fromList acc))
    _ -> do !t <- decodeValue
            decodeListN (n-1) (t : acc)

decodeListIndefLen :: [Value] -> Decoder s Value
decodeListIndefLen acc = do
  stop <- decodeBreakOr
  if stop then return $! DagArray (V.reverse (V.fromList acc))
          else do !tm <- decodeValue
                  decodeListIndefLen (tm : acc)

decodeNumberIntegral :: Decoder s Value
decodeNumberIntegral = DagNumber . fromInteger <$> decodeInteger

decodeNumberFloating :: Decoder s Value
decodeNumberFloating = DagNumber . fromFloatDigits <$> decodeDouble

decodeMapN :: Int -> [(Text, Value)] -> Decoder s Value
decodeMapN !n acc =
    case n of
      0 -> return $! DagObject (HashMap.fromList acc)
      _ -> do !tm   <- decodeString
              !tm'  <- decodeValue
              decodeMapN (n-1) ((tm, tm') : acc)

encodeValue :: Value -> Encoding
encodeValue = \case
  LinkValue lnk  -> encodeMerkleLink lnk
  DagObject hmap ->
    let ts = HashMap.toList hmap
    in encodeMapLen (fromIntegral $ length ts)
       <> mconcat [ encodeString t <> encodeValue t' | (t, t') <- ts ]
  DagArray  arr  -> encodeListLen (fromIntegral $ length arr)
                 <> mconcat [ encodeValue t | t <- V.toList arr ]
  TextValue text -> encodeString text
  -- This instance is taken from a cbor example
  DagNumber num  -> case floatingOrInteger num of
    Left  d -> encodeDouble d
    Right i -> encodeInteger i
  DagBool   bool -> encodeBool bool
  Null           -> encodeNull

decodeValue :: Decoder s Value
decodeValue = do
  tkty <- peekTokenType
  case tkty of
    TypeTag -> do
      _tag <- decodeTag
      LinkValue <$> decodeMerkleLink
    TypeString  -> TextValue <$> decodeString
    TypeListLen -> decodeListLen      >>= flip decodeListN []
    TypeListLen64    -> decodeListLen      >>= flip decodeListN []
    TypeListLenIndef -> decodeListIndefLen []
    TypeMapLen  -> decodeMapLen >>= flip decodeMapN []
    TypeBool    -> DagBool   <$> decodeBool
    TypeNull    -> Null      <$  decodeNull

    TypeUInt    -> decodeNumberIntegral
    TypeUInt64  -> decodeNumberIntegral
    TypeNInt    -> decodeNumberIntegral
    TypeNInt64  -> decodeNumberIntegral
    TypeInteger -> decodeNumberIntegral
    TypeFloat64 -> decodeNumberFloating

    _ -> fail $ "unexpected CBOR token type for IPLD value: " ++ show tkty

instance IsString Value where
  fromString = TextValue . fromString

data Row
  -- TODO: should LinkRow exist?
  = LinkRow MerkleLink
  | ValueRow Text Value
  deriving (Eq, Show, Generic)

type instance Index Value = Text

type instance IxValue Value = Value
instance Ixed Value where
  ix i f (DagObject o) = DagObject <$> ix i f o
  ix _ _ v             = pure v
  {-# INLINE ix #-}

instance Plated Value

valueCid :: Value -> Cid
valueCid = mkCid . toStrictByteString . encodeValue

linkToM :: Value -> MerkleLink
linkToM = MerkleLink . valueCid

linkToV :: Value -> Value
linkToV = LinkValue . linkToM

-- type MerkleUniverse = HashMap MerkleLink Value
-- type GraftM = WriterT Text (State MerkleUniverse)

-- Options:
--   - what to do when we don't have the link value?
--   - graft all links?
--   - graft grafted-in values? (recursively graft (link) until no links left?
--
-- graftAll :: Value -> GraftM (Maybe Value)

graft :: (MerkleLink -> Value) -> Value -> Value
graft f = runIdentity . graftM (Identity . f)

graftM :: Monad m => (MerkleLink -> m Value) -> Value -> m Value
graftM f = transformM $ \case
  LinkValue lnk -> f lnk
  other         -> pure other

link :: (MerkleLink -> Value) -> Value -> Value
link f = runIdentity . linkM (Identity . f)

linkM :: Monad m => (MerkleLink -> m Value) -> Value -> m Value
linkM f = rewriteM $ \case
  LinkValue lnk -> Just <$> f lnk
  _             -> pure Nothing

newtype RelMerklePath = RelMerklePath [Text]
  deriving (Eq, Show)

-- | A merkle-path consisting of
-- * merkle-link
-- * path traversal
data AbsMerklePath = AbsMerklePath MerkleLink RelMerklePath
  deriving Show

data TraversalResult a
  = Found a
  -- | Yield (a, MerklePath)

  -- "cross-object traversal"
  | Yield MerkleLink RelMerklePath
  | KeyNotFound a RelMerklePath
  deriving (Eq, Show)

traverseValue :: RelMerklePath -> Value -> TraversalResult Value
traverseValue (RelMerklePath []) v = Found v
traverseValue path@(RelMerklePath (a:as)) arr@(DagArray vals) =
  let mayVal = do
        i <- readMaybe (T.unpack a)
        vals ^? ix i
  in case mayVal of
       Just val -> traverseValue (RelMerklePath as) val
       Nothing -> KeyNotFound arr path
traverseValue path@(RelMerklePath (a:as)) obj@(DagObject vals)
  = case vals ^? ix a of
      Just val -> traverseValue (RelMerklePath as) val
      Nothing -> KeyNotFound obj path
traverseValue path (LinkValue lnk) = Yield lnk path
traverseValue path val = KeyNotFound val path


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
  Right lnk -> Just lnk

-- merkleLink' :: Text -> Maybe Value
-- merkleLink' text = LinkValue <$> merkleLink text

-- $parsing

-- eg:
--   "/ipfs/QmUmg7BZC1YP1ca66rRtWKxpXp77WgVHrnv263JtDuvs2k/a/b/c"
--   ->
--   AbsMerklePath ["QmUmg7BZC1YP1ca66rRtWKxpXp77WgVHrnv263JtDuvs2k", "a", "b", "c"]
--
absMerklePath :: Text -> Either String AbsMerklePath
absMerklePath = parseOnly (parseAbsMerklePath <* endOfInput)

parseAbsMerklePath :: Parser AbsMerklePath
parseAbsMerklePath = do
  lnk <- parseMerkleLink
  traversal <- (do
    _ <- char '/'
    takeWhile (/= '/') `sepBy` char '/'
    ) <|> pure []
  pure (AbsMerklePath lnk (RelMerklePath traversal))

-- "currently, only the ipfs hierarchy is allowed"
-- TODO: allow option for "/ipfs" or not
parseMerkleLink :: Parser MerkleLink
parseMerkleLink = MerkleLink <$> parseIpfsMultiaddr

-- TODO: for now we don't parse multihashes in full generality -- we only allow
-- one protocol / location pair. In fact, we only allow /ipfs/. And we don't
-- try to hard on verifying the identifier is okay.
parseIpfsMultiaddr :: Parser Cid
parseIpfsMultiaddr = do
  _proto <- string "/ipfs"
  _ <- char '/'
  rest <- takeText
  case ABS.parseOnly parseCid (encodeUtf8 rest) of
    Left err -> fail err
    Right x -> pure x

readCid :: SBS.ByteString -> Maybe Cid
readCid str = case ABS.parseOnly parseCid str of
  Left _err -> Nothing
  Right x   -> Just x

base58Class :: String
base58Class = B8.unpack $ unAlphabet bitcoinAlphabet

parseCid :: ABS.Parser Cid
parseCid = do
  base <- ABS.anyWord8
  case toEnum8 base of
    Base58Btc -> do
      b58str <- BS.pack <$> ABS.count 48 (ABS.satisfy (ABS.inClass base58Class))
      case decodeBase58 bitcoinAlphabet b58str of
        Nothing -> fail "invalid base-58 string"
        Just byteStr -> do
          let (header, hashVal) = BS.splitAt 2 byteStr
          [version, codec] <- pure (BS.unpack header)
          case (version, toEnum8 codec) of
            (1, DagCbor) ->
              case ABS.parseOnly parseMultihash hashVal of
                Left err -> fail err
                Right mh -> pure $ Cid Base58Btc 1 DagCbor mh
            _ -> fail "Can't currently parse other than cid v1, dag-cbor"
    _ -> fail "Can't currently parse other than base58btc"

toEnum8 :: Enum a => Word8 -> a
toEnum8 = toEnum . w8ToI

parseMultihash :: ABS.Parser Multihash
parseMultihash = do
  fun  <- ABS.anyWord8
  size <- ABS.anyWord8
  rest <- ABS.takeByteString
  let len = BS.length rest
  when (len /= 32) $ fail $
       "invalid multihash length ("
    ++ show len
    ++ ") ("
    ++ show (Hex.encode rest)
    ++ ")"
  pure (Multihash (toEnum8 fun) (toEnum8 size) rest)
