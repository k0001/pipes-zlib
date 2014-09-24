{-# LANGUAGE RankNTypes #-}

-- | This module exports utilities to compress and decompress GZip @pipes@
-- streams.

module Pipes.GZip (
  -- * Streams
    decompress
  , compress

  -- * Compression level
  -- $ccz-re-export
  , ZC.defaultCompression
  , ZC.noCompression
  , ZC.bestSpeed
  , ZC.bestCompression
  , ZC.compressionLevel
  ) where

import qualified Codec.Compression.Zlib    as ZC
import qualified Data.ByteString           as B
import           Pipes
import qualified Pipes.Zlib

--------------------------------------------------------------------------------

-- | Decompress bytes flowing from a 'Producer'.
decompress
  :: MonadIO m
  => Producer' B.ByteString m r -- ^ Compressed stream
  -> Producer' B.ByteString m r -- ^ Decompressed stream
decompress = Pipes.Zlib.decompress (ZC.WindowBits 31)
{-# INLINABLE decompress #-}


-- | Compress bytes flowing from a 'Producer'.
compress
  :: MonadIO m
  => ZC.CompressionLevel
  -> Producer' B.ByteString m r -- ^ Decompressed stream
  -> Producer' B.ByteString m r -- ^ Compressed stream
compress clevel = Pipes.Zlib.compress clevel (ZC.WindowBits 31)
{-# INLINABLE compress #-}
