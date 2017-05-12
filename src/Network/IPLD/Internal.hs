{-# language BangPatterns #-}
{-# language DeriveGeneric #-}
{-# language LambdaCase #-}
{-# language OverloadedStrings #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language DeriveDataTypeable #-}
{-# language TypeFamilies #-}

module Network.IPLD.Internal where

import Prelude hiding (takeWhile)

import           Control.Applicative
import           Control.Lens
  ((^?), Index, IxValue, Ixed(..), transformM, Plated, rewriteM)
import           Control.Monad (when)
import           Control.Monad.State
import           Control.Monad.Writer
import           Data.ByteString.Base58
import           Data.ByteString.Lazy (toStrict)
import           Data.Data
import           Data.Functor.Identity
import           Data.HashMap.Strict (HashMap)
import           Data.Hashable
import           Data.Monoid ((<>))
import           Data.String
import           Data.Text (Text)
import           Data.Text.Encoding (encodeUtf8)
import           Data.Vector (Vector)
import           Data.Word (Word8)
import           GHC.Generics
import           Text.Read
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as Hex
import qualified Data.ByteString.Char8 as B8
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as T
import qualified Data.Vector as V

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

newtype MerkleLink = MerkleLink Cid -- MultihashDigest
  deriving (Eq, Show, Generic, Hashable, Typeable, Data)

instance Serialise MerkleLink where
  encode (MerkleLink loc) = encodeTag cborTagLink <> encode loc
  decode = decodeTag *> decode

instance Serialise Value where
  encode = \case
    LinkValue lnk  -> encode lnk
    TextValue text -> encode text
    DagObject hmap -> encode hmap
    DagArray  arr  -> encode arr

  decode = do
    tkty <- peekTokenType
    case tkty of
      TypeTag -> do
        _tag <- decodeTag
        LinkValue <$> decode
      TypeString -> TextValue <$> decode
      TypeListLen -> DagArray <$> decode
      TypeMapLen -> DagObject <$> decode
      _ -> fail $ "unexpected CBOR token type for IPLD value: " ++ show tkty

data Hierarchy = Ipfs

data Row
  -- TODO: should LinkRow exist?
  = LinkRow MerkleLink
  | ValueRow Text Value
  deriving (Eq, Show, Generic)

data Value
  = LinkValue MerkleLink
  | DagObject (HashMap Text Value)
  | DagArray (Vector Value)
  | TextValue Text -- TODO should this be a bytestring?
  -- Should there also be number, bool, null?
  deriving (Eq, Show, Generic, Typeable, Data)

type instance Index Value = Text

type instance IxValue Value = Value
instance Ixed Value where
  ix i f (DagObject o) = DagObject <$> ix i f o
  ix _ _ v             = pure v
  {-# INLINE ix #-}

instance Plated Value

linkToM :: Value -> MerkleLink
linkToM = MerkleLink . mkCid . toStrict . serialise

linkToV :: Value -> Value
linkToV = LinkValue . linkToM

type MerkleUniverse = HashMap MerkleLink Value
type GraftM = WriterT Text (State MerkleUniverse)

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
  other          -> pure other

link :: (MerkleLink -> Value) -> Value -> Value
link f = runIdentity . linkM (Identity . f)

linkM :: Monad m => (MerkleLink -> m Value) -> Value -> m Value
linkM f = rewriteM $ \case
  LinkValue lnk -> Just <$> f lnk
  _              -> pure Nothing

instance IsString Value where
  fromString = TextValue . fromString

-- | A merkle-path consisting of
-- * merkle-link
-- * path traversal
data MerklePath = MerklePath MerkleLink [Text]
  deriving Show

data TraversalResult a
  = Found a
  -- | Yield (a, MerklePath)

  -- "cross-object traversal"
  | Yield MerkleLink [Text]
  | KeyNotFound a [Text]
  deriving (Eq, Show)

traverseValue :: [Text] -> Value -> TraversalResult Value
traverseValue [] v = Found v
traverseValue path@(a:as) arr@(DagArray vals) =
  let mayVal = do
        i <- readMaybe (T.unpack a)
        vals ^? ix i
  in case mayVal of
       Just val -> traverseValue as val
       Nothing -> KeyNotFound arr path
traverseValue path@(a:as) obj@(DagObject vals) = case vals ^? ix a of
   Just val -> traverseValue as val
   Nothing -> KeyNotFound obj path
traverseValue path val@(TextValue _) = KeyNotFound val path
traverseValue path (LinkValue lnk) = Yield lnk path


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

merkleLink' :: Text -> Maybe Value
merkleLink' text = LinkValue <$> merkleLink text

-- $parsing

-- eg:
--   "/ipfs/QmUmg7BZC1YP1ca66rRtWKxpXp77WgVHrnv263JtDuvs2k/a/b/c"
--   ->
--   MerklePath ["QmUmg7BZC1YP1ca66rRtWKxpXp77WgVHrnv263JtDuvs2k", "a", "b", "c"]
--
merklePath :: Text -> Either String MerklePath
merklePath = parseOnly (parseMerklePath <* endOfInput)

parseMerklePath :: Parser MerklePath
parseMerklePath = do
  lnk <- parseMerkleLink
  traversal <- (do
    _ <- char '/'
    takeWhile (not . (== '/')) `sepBy` (char '/')
    ) <|> pure []
  pure (MerklePath lnk traversal)

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
            (1, DagCbor) -> do
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

  -- TODO: more expressive failure info
  fromIpld :: Value -> Maybe a

instance IsIpld Value where
  toIpld = id
  fromIpld = Just