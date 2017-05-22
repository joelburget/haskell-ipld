{-# language GeneralizedNewtypeDeriving #-}
{-# language OverloadedStrings #-}
module Network.IPLD.Client
  ( Channel
  , get
  , put
  , pin
  , unpin
  , publish
  , subscribe
  ) where

import qualified Control.Foldl as Fold
import qualified Data.Attoparsec.ByteString as ABS
import           Data.Text.Encoding (decodeUtf8, encodeUtf8)
import           Turtle
import qualified Turtle.Bytes as TB

import Network.IPLD.Cid
import Network.IPLD.Internal

get :: Cid -> IO (Either String Value)
get cid = do
  let cmd = "ipfs dag get " <> decodeUtf8 (compact cid)
  (exitCode, out, _stderr) <- shellStrictWithErr cmd empty
  case exitCode of
    ExitFailure i -> pure (Left $ "bad exit code: " ++ show i)
    ExitSuccess -> case jsonDecode (encodeUtf8 out) of
      Nothing -> pure (Left "couldn't decode")
      Just val -> pure (Right val)

put :: Value -> IO (Either String Cid)
put val = do
  rawLine <- fold
    (TB.inshell "ipfs dag put -" (pure (jsonEncode val)))
    Fold.mconcat
  pure $ ABS.parseOnly parseCid rawLine

pin :: Cid -> IO (Either String ())
pin cid = do
  let cmd = "ipfs pin " <> decodeUtf8 (compact cid)
  exitCode <- TB.shell cmd empty
  case exitCode of
    ExitFailure i -> pure (Left $ "bad exit code: " ++ show i)
    ExitSuccess -> pure (Right ())

-- TODO: There are multiple types of pins -- do we expose?
unpin :: Cid -> IO (Either String ())
unpin cid = do
  let cmd = "ipfs pin rm -r " <> decodeUtf8 (compact cid)
  exitCode <- TB.shell cmd empty
  case exitCode of
    ExitFailure i -> pure (Left $ "bad exit code: " ++ show i)
    ExitSuccess -> pure (Right ())

newtype Channel = Channel Text
  deriving (Eq, Ord, Show, Monoid, IsString)

publish :: Channel -> Text -> IO (Either String ())
publish (Channel chan) msg = do
  -- stick a newline on the end so it shows up as a distinct line on the other
  -- end
  let cmd = "ipfs pubsub pub " <> chan <> " " <> msg <> "\n"
  exitCode <- TB.shell cmd empty
  case exitCode of
    ExitFailure i -> pure (Left $ "bad exit code: " ++ show i)
    ExitSuccess -> pure (Right ())

subscribe :: Channel -> Shell Line
subscribe (Channel chan) =
  let cmd = "ipfs pubsub sub " <> chan
  in inshell cmd mempty

-- checkIpfs :: IO IpfsStatus
