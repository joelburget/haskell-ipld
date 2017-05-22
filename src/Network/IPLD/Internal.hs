{-# language CPP #-}
{-# language DataKinds #-}
{-# language DefaultSignatures #-}
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
{-# language ViewPatterns #-}
{-# options_ghc -funbox-strict-fields #-}

module Network.IPLD.Internal
  ( MerkleLink(..)
  , AbsMerklePath(..)
  , RelMerklePath(..)
  , Value(..)
  , Row(..)
  , TraversalResult(..)
  , IsIpld(..)
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
  ) where

import Prelude hiding (takeWhile)

import           Control.Applicative
import           Control.Lens
  ((^?), Index, IxValue, Ixed(..), transformM, Plated, rewriteM)
import           Control.Monad (when)
import           Control.Monad.State.Strict
-- import           Control.Monad.Writer
import qualified Data.Aeson as Aeson
import           Data.ByteString.Base58
import           Data.ByteString.Lazy (toStrict)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString as SBS
import           Data.Data
import           Data.Functor.Identity
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import           Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import           Data.Hashable
import           Data.Int
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
import qualified Data.Vector.Mutable as VM

import           Data.Attoparsec.Text
import qualified Data.Attoparsec.ByteString as ABS

import           Data.Binary.Serialise.CBOR
import           Data.Binary.Serialise.CBOR.Encoding
import           Data.Binary.Serialise.CBOR.Decoding

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

instance Serialise MerkleLink where
  encode (MerkleLink loc) = encodeTag cborTagLink <> encode loc
  decode = decodeTag *> decode

decodeIndefList :: Decoder s Value
decodeIndefList = DagArray . V.fromList <$> decode

decodeNumberIntegral :: Decoder s Value
decodeNumberIntegral = DagNumber . fromInteger <$> decode

decodeNumberFloating :: Decoder s Value
decodeNumberFloating = DagNumber . fromFloatDigits <$>
  (decode :: Decoder s Double)

instance Serialise Value where
  encode = \case
    LinkValue lnk  -> encode lnk
    DagObject hmap -> encode hmap
    DagArray  arr  -> encode arr
    TextValue text -> encode text
    -- This instance is taken from a cbor example
    DagNumber num  -> case floatingOrInteger num of
      Left  d -> encode (d :: Double)
      Right i -> encode (i :: Integer)
    DagBool   bool -> encode bool
    Null           -> encodeNull

  decode = do
    tkty <- peekTokenType
    case tkty of
      TypeTag -> do
        _tag <- decodeTag
        LinkValue <$> decode
      TypeString  -> TextValue <$> decode
      TypeListLen -> DagArray  <$> decode
      TypeListLenIndef -> decodeIndefList
      TypeMapLen  -> DagObject <$> decode
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
valueCid = mkCid . toStrict . serialise

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

-- $class

class IsIpld a where
  toIpld :: a -> Value

  default toIpld :: (Generic a, GToIpld (Rep a)) => a -> Value
  toIpld = gToIpld . from

  -- TODO: more expressive failure info
  fromIpld :: Value -> Maybe a

  default fromIpld :: (Generic a, GFromIpld (Rep a)) => Value -> Maybe a
  fromIpld = to <$$> gFromIpld

  cidOf :: a -> Cid
  cidOf = valueCid . toIpld

instance IsIpld Value where
  toIpld = id
  fromIpld = Just

instance IsIpld Bool
instance IsIpld Char where
  toIpld c = TextValue (T.pack [c])
  fromIpld = \case
    (TextValue (T.unpack -> [c])) -> Just c
    _ -> Nothing

#define IntInstance(I)                 \
instance IsIpld I where {              \
  toIpld = DagNumber . fromIntegral;   \
  fromIpld = \case {                   \
    DagNumber n -> toBoundedInteger n; \
    _ -> Nothing;                      \
  }                                    \
}

IntInstance(Int)
IntInstance(Int8)
IntInstance(Int16)
IntInstance(Int32)
IntInstance(Int64)
IntInstance(Word8)
IntInstance(Word16)
IntInstance(Word32)
IntInstance(Word64)

#define FloatingInstance(I)                       \
instance IsIpld I where {                         \
  toIpld = DagNumber . fromFloatDigits;           \
  fromIpld = \case {                              \
    DagNumber n -> case toBoundedRealFloat n of { \
      Left _ -> Nothing;                          \
      Right f -> Just f;                          \
    };                                            \
    _ -> Nothing;                                 \
  }                                               \
}

FloatingInstance(Double)
FloatingInstance(Float)

-- instance IsIpld Integer
-- instance IsIpld Ordering

instance IsIpld Text where
  toIpld = TextValue
  fromIpld = \case
    TextValue str -> Just str
    _             -> Nothing

instance {-# OVERLAPPING #-} IsIpld String where
  toIpld = TextValue . T.pack
  fromIpld = \case
    TextValue t -> Just (T.unpack t)
    _           -> Nothing

instance IsIpld MerkleLink where
  toIpld = LinkValue
  fromIpld = \case
    LinkValue l -> Just l
    _           -> Nothing

instance IsIpld Scientific where
  toIpld = DagNumber
  fromIpld = \case
    DagNumber s -> Just s
    _           -> Nothing

instance (IsIpld a, Eq a, Hashable a) => IsIpld (HashSet a) where
  toIpld = DagArray . fmap toIpld . V.fromList . HashSet.toList
  fromIpld = \case
    DagArray v -> HashSet.fromList . V.toList <$> sequence (fromIpld <$> v)
    _ -> Nothing

-- TODO IsIpldKey?
instance IsIpld a => IsIpld (HashMap Text a) where
  toIpld = DagObject . fmap toIpld
  fromIpld = \case
    DagObject o -> sequence $ fromIpld <$> o
    _ -> Nothing

instance IsIpld ()

-- TODO: serialize tuples as arrays
instance (IsIpld a, IsIpld b) => IsIpld (a, b) where
  toIpld (a, b) = DagArray $ V.create $ do
    mv <- VM.unsafeNew 2
    VM.unsafeWrite mv 0 (toIpld a)
    VM.unsafeWrite mv 1 (toIpld b)
    return mv
  {-# INLINE toIpld #-}

  fromIpld = \case
    DagArray (V.toList -> [a, b]) -> (,) <$> fromIpld a <*> fromIpld b
    _ -> Nothing
  {-# INLINE fromIpld #-}

instance (IsIpld a, IsIpld b, IsIpld c) => IsIpld (a, b, c) where
  toIpld (a, b, c) = DagArray $ V.create $ do
    mv <- VM.unsafeNew 3
    VM.unsafeWrite mv 0 (toIpld a)
    VM.unsafeWrite mv 1 (toIpld b)
    VM.unsafeWrite mv 2 (toIpld c)
    return mv
  {-# INLINE toIpld #-}

  fromIpld = \case
    DagArray (V.toList -> [a, b, c]) ->
      (,,) <$> fromIpld a <*> fromIpld b <*> fromIpld c
    _ -> Nothing
  {-# INLINE fromIpld #-}

instance (IsIpld a, IsIpld b, IsIpld c, IsIpld d) => IsIpld (a, b, c, d) where
  toIpld (a, b, c, d) = DagArray $ V.create $ do
    mv <- VM.unsafeNew 4
    VM.unsafeWrite mv 0 (toIpld a)
    VM.unsafeWrite mv 1 (toIpld b)
    VM.unsafeWrite mv 2 (toIpld c)
    VM.unsafeWrite mv 3 (toIpld d)
    return mv
  {-# INLINE toIpld #-}

  fromIpld = \case
    DagArray (V.toList -> [a, b, c, d]) ->
      (,,,) <$> fromIpld a <*> fromIpld b <*> fromIpld c <*> fromIpld d
    _ -> Nothing
  {-# INLINE fromIpld #-}

instance (IsIpld a, IsIpld b, IsIpld c, IsIpld d, IsIpld e)
  => IsIpld (a, b, c, d, e) where
  toIpld (a, b, c, d, e) = DagArray $ V.create $ do
    mv <- VM.unsafeNew 5
    VM.unsafeWrite mv 0 (toIpld a)
    VM.unsafeWrite mv 1 (toIpld b)
    VM.unsafeWrite mv 2 (toIpld c)
    VM.unsafeWrite mv 3 (toIpld d)
    VM.unsafeWrite mv 4 (toIpld e)
    return mv
  {-# INLINE toIpld #-}

  fromIpld = \case
    DagArray (V.toList -> [a, b, c, d, e]) ->
      (,,,,) <$> fromIpld a <*> fromIpld b <*> fromIpld c <*> fromIpld d
             <*> fromIpld e
    _ -> Nothing
  {-# INLINE fromIpld #-}

instance (IsIpld a, IsIpld b, IsIpld c, IsIpld d, IsIpld e, IsIpld f)
  => IsIpld (a, b, c, d, e, f) where
  toIpld (a, b, c, d, e, f) = DagArray $ V.create $ do
    mv <- VM.unsafeNew 6
    VM.unsafeWrite mv 0 (toIpld a)
    VM.unsafeWrite mv 1 (toIpld b)
    VM.unsafeWrite mv 2 (toIpld c)
    VM.unsafeWrite mv 3 (toIpld d)
    VM.unsafeWrite mv 4 (toIpld e)
    VM.unsafeWrite mv 5 (toIpld f)
    return mv
  {-# INLINE toIpld #-}

  fromIpld = \case
    DagArray (V.toList -> [a, b, c, d, e, f]) ->
      (,,,,,) <$> fromIpld a <*> fromIpld b <*> fromIpld c <*> fromIpld d
              <*> fromIpld e <*> fromIpld f
    _ -> Nothing
  {-# INLINE fromIpld #-}

instance (IsIpld a, IsIpld b, IsIpld c, IsIpld d, IsIpld e, IsIpld f, IsIpld g)
  => IsIpld (a, b, c, d, e, f, g) where
  toIpld (a, b, c, d, e, f, g) = DagArray $ V.create $ do
    mv <- VM.unsafeNew 7
    VM.unsafeWrite mv 0 (toIpld a)
    VM.unsafeWrite mv 1 (toIpld b)
    VM.unsafeWrite mv 2 (toIpld c)
    VM.unsafeWrite mv 3 (toIpld d)
    VM.unsafeWrite mv 4 (toIpld e)
    VM.unsafeWrite mv 5 (toIpld f)
    VM.unsafeWrite mv 6 (toIpld g)
    return mv
  {-# INLINE toIpld #-}

  fromIpld = \case
    DagArray (V.toList -> [a, b, c, d, e, f, g]) ->
      (,,,,,,) <$> fromIpld a <*> fromIpld b <*> fromIpld c <*> fromIpld d
               <*> fromIpld e <*> fromIpld f <*> fromIpld g
    _ -> Nothing
  {-# INLINE fromIpld #-}

instance (IsIpld a, IsIpld b) => IsIpld (Either a b)

instance IsIpld a => IsIpld [a] where
  toIpld = DagArray . V.fromList . fmap toIpld
  fromIpld = \case
    DagArray v -> sequence $ fromIpld <$> V.toList v
    _ -> Nothing

instance IsIpld a => IsIpld (Vector a) where
  toIpld = DagArray . fmap toIpld
  fromIpld = \case
    DagArray v -> sequence $ fromIpld <$> v
    _ -> Nothing

instance IsIpld a => IsIpld (Maybe a)

instance IsIpld Cid where
  toIpld = TextValue . decodeUtf8 . compact
  fromIpld = \case
    LinkValue (MerkleLink cid) -> Just cid
    -- TextValue str -> case ABS.parseOnly parseCid (encodeUtf8 str) of
    --   Right cid -> Just cid
    --   Left _err -> Nothing
    _ -> Nothing

--------------------------------------------------------------------------------
-- Generic instances

-- Quote:
--
-- Factored into two classes because this makes GHC optimize the
-- instances faster. This doesn't matter for builds of binary, but it
-- matters a lot for end-users who write 'instance Binary T'. See
-- also: https://ghc.haskell.org/trac/ghc/ticket/9630

class GToIpld f where
  gToIpld :: f a -> Value

class GFromIpld f where
  gFromIpld :: Value -> Maybe (f a)

instance GToIpld V1 where
  -- Data types without constructors are still serialised as null value
  -- TODO: fact check this
  gToIpld _ = Null

instance GFromIpld V1 where
  gFromIpld = error "V1 don't have contructors" <$ pure ()

instance GToIpld U1 where
  -- Constructors without fields are serialised as an empty array
  gToIpld _ = array []

instance GFromIpld U1 where
  gFromIpld = \case
    DagArray (V.null -> True) -> pure U1
    _                         -> Nothing

instance GToIpld a => GToIpld (M1 i c a) where
  gToIpld = gToIpld . unM1

instance GFromIpld a => GFromIpld (M1 i c a) where
  gFromIpld = M1 <$$> gFromIpld

instance IsIpld f => GToIpld (K1 i f) where
  gToIpld = toIpld . unK1

instance IsIpld f => GFromIpld (K1 i f) where
  gFromIpld = K1 <$$> fromIpld

instance (GIsIpldProd f, GIsIpldProd g) => GToIpld (f :*: g) where
  gToIpld (f :*: g) = DagArray $ toIpldSeq f <> toIpldSeq g

instance (GIsIpldProd f, GIsIpldProd g) => GFromIpld (f :*: g) where
  gFromIpld ipld = do
    DagArray fields <- pure ipld
    let action = (:*:) <$> gFromIpldSeq <*> gFromIpldSeq
    evalStateT action fields

instance (GIsIpldSum f, GIsIpldSum g) => GToIpld (f :+: g) where
  gToIpld a = DagArray $
    V.singleton (DagNumber (fromIntegral (conNumber a))) <> toIpldSum a

instance (GIsIpldSum f, GIsIpldSum g) => GFromIpld (f :+: g) where
  gFromIpld ipld = do
    DagArray v <- pure ipld
    let len = V.length v
    when (len == 0) $ fail "Empty list encountered for sum type"
    DagNumber n <- pure $ V.head v
    n' <- toBoundedInteger n
    trueLen <- fieldsForCon (Proxy :: Proxy (f :+: g)) n'
    when (len /= fromIntegral trueLen + 1) $ fail $
      "Number of fields mismatch: expected="++show trueLen++" got="++show len
    evalStateT (fromIpldSum n') (V.tail v)

-- | Serialization of product types
class GIsIpldProd f where
  -- | Number of fields in product type
  nFields   :: Proxy f -> Word
  -- | Encode sequentially
  toIpldSeq :: f a -> Vector Value
  -- | Decode sequentially
  gFromIpldSeq :: StateT (Vector Value) Maybe (f a)

instance (GIsIpldProd f, GIsIpldProd g) => GIsIpldProd (f :*: g) where
  nFields _ = nFields (Proxy :: Proxy f) + nFields (Proxy :: Proxy g)
  toIpldSeq (f :*: g) = toIpldSeq f <> toIpldSeq g
  gFromIpldSeq = (:*:) <$> gFromIpldSeq <*> gFromIpldSeq

instance GIsIpldProd U1 where
  -- N.B. Could only be reached when one of constructors in sum type
  --      don't have parameters
  nFields   _  = 0
  toIpldSeq _  = mempty
  gFromIpldSeq = pure U1

instance IsIpld a => GIsIpldProd (K1 i a) where
  -- Ordinary field
  nFields    _     = 1
  toIpldSeq (K1 f) = V.singleton (toIpld f)
  gFromIpldSeq     = do
    vals <- get
    put (V.tail vals)
    -- TODO: safe
    let Just result = fromIpld (V.head vals)
    pure $ K1 result

instance (i ~ S, GIsIpldProd f) => GIsIpldProd (M1 i c f) where
  -- We skip metadata
  nFields     _     = 1
  toIpldSeq  (M1 f) = toIpldSeq f
  gFromIpldSeq      = M1 <$> gFromIpldSeq

-- | Serialization of sum types
class GIsIpldSum f where
  -- | Number of constructor of given value
  conNumber   :: f a -> Word
  -- | Number of fields of given value
  numOfFields :: f a -> Word
  -- | Encode field
  toIpldSum   :: f a  -> Vector Value

  -- | Decode field
  fromIpldSum   :: Word -> StateT (Vector Value) Maybe (f a)
  -- | Number of constructors
  nConstructors :: Proxy f -> Word
  -- | Number of fields for given constructor number
  fieldsForCon  :: Proxy f -> Word -> Maybe Word

instance (GIsIpldSum f, GIsIpldSum g) => GIsIpldSum (f :+: g) where
  conNumber = \case
    L1 f -> conNumber f
    R1 g -> conNumber g + nConstructors (Proxy :: Proxy f)
  numOfFields = \case
    L1 f -> numOfFields f
    R1 g -> numOfFields g
  toIpldSum = \case
    L1 f -> toIpldSum f
    R1 g -> toIpldSum g

  nConstructors _ = nConstructors (Proxy :: Proxy f)
                  + nConstructors (Proxy :: Proxy g)

  fieldsForCon _ n | n < nL    = fieldsForCon (Proxy :: Proxy f) n
                   | otherwise = fieldsForCon (Proxy :: Proxy g) (n - nL)
    where
      nL = nConstructors (Proxy :: Proxy f)

  fromIpldSum nCon | nCon < nL = L1 <$> fromIpldSum nCon
                   | otherwise = R1 <$> fromIpldSum (nCon - nL)
    where
      nL = nConstructors (Proxy :: Proxy f)

instance (i ~ C, GIsIpldProd f) => GIsIpldSum (M1 i c f) where
  conNumber    _     = 0
  numOfFields  _     = nFields (Proxy :: Proxy f)
  toIpldSum   (M1 f) = toIpldSeq f

  nConstructors  _ = 1
  fieldsForCon _ 0 = return $ nFields (Proxy :: Proxy f)
  fieldsForCon _ _ = fail "Bad constructor number"
  fromIpldSum 0    = M1 <$> gFromIpldSeq
  fromIpldSum _    = fail "bad constructor number"
