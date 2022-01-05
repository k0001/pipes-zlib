{-# LANGUAGE RankNTypes #-}

-- | This module exports utilities to compress and decompress GZip @pipes@
-- streams.

module Pipes.GZip
  ( -- * Streams
    decompress
  , decompressMember
  , compress

  -- * Compression level
  , Pipes.Zlib.CompressionLevel
  , Pipes.Zlib.defaultCompression
  , Pipes.Zlib.noCompression
  , Pipes.Zlib.bestSpeed
  , Pipes.Zlib.bestCompression
  , Pipes.Zlib.compressionLevel
  ) where

import Data.Function (fix)
import qualified Data.Streaming.Zlib as Zlib
import qualified Data.ByteString as B
import Pipes
import qualified Pipes.Zlib

--------------------------------------------------------------------------------

-- | Decompress bytes flowing from a 'Producer'.
--
-- Throws 'UnexpectedEndOfInput' if the compressed stream ends prematurely.
decompress
  :: MonadIO m
  => Producer B.ByteString m r -- ^ Compressed stream
  -> Producer' B.ByteString m r -- ^ Decompressed stream
decompress = fix $ \k p -> do
  ebs <- decompressMember p
  case ebs of
    Right s -> pure s
    Left  b -> k b
{-# INLINABLE decompress #-}

-- | Decompress bytes flowing from a 'Producer', returning any leftover input
-- that follows the first member of the compressed stream.
--
-- The gzip format allows a single archive to be made up of several smaller
-- archives ("members") concatenated together.  In such cases this function
-- decompresses only the first member encountered: further members, if any,
-- remain in the leftover input.
decompressMember
  :: MonadIO m
  => Producer B.ByteString m r -- ^ Compressed stream
  -> Producer' B.ByteString m (Either (Producer B.ByteString m r) r)
     -- ^ Decompressed stream, returning either a 'Producer' of the leftover input
     -- or the return value from the input 'Producer'.
decompressMember = Pipes.Zlib.decompress' gzWindowBits
{-# INLINABLE decompressMember #-}


-- | Compress bytes flowing from a 'Producer'.
compress
  :: MonadIO m
  => Pipes.Zlib.CompressionLevel
  -> Producer B.ByteString m r -- ^ Decompressed stream
  -> Producer' B.ByteString m r -- ^ Compressed stream
compress clevel = Pipes.Zlib.compress clevel gzWindowBits
{-# INLINABLE compress #-}

gzWindowBits :: Zlib.WindowBits
gzWindowBits = Zlib.WindowBits 31
