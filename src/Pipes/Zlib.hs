{-# LANGUAGE RankNTypes #-}

-- | This module exports utilities to compress and decompress @pipes@ streams
-- using the zlib compression codec.
--
-- If you want to compress or decompress GZip streams, use the "Pipes.GZip"
-- module instead.

module Pipes.Zlib (
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

  -- * Window size
  -- $ccz-re-export
  , ZC.defaultWindowBits
  , ZC.windowBits
  ) where

import qualified Codec.Compression.Zlib    as ZC
import qualified Codec.Zlib                as Z
import           Control.Monad             (unless)
import qualified Data.ByteString           as B
import           Pipes

--------------------------------------------------------------------------------

-- | Decompress bytes flowing from a 'Producer'.
--
-- See the "Codec.Compression.Zlib" module for details about 'Z.WindowBits'.
--
-- @
-- 'decompress' :: 'MonadIO' m
--            => 'ZC.WindowBits'
--            => 'Producer' 'B.ByteString' m r
--            -> 'Producer' 'B.ByteString' m r
-- @
decompress
  :: MonadIO m
  => ZC.WindowBits
  -> Proxy x' x () B.ByteString m r -- ^ Compressed stream
  -> Proxy x' x () B.ByteString m r -- ^ Decompressed stream
decompress wbits p0 = do
    inf <- liftIO $ Z.initInflate wbits
    r <- for p0 $ \bs -> do
       popper <- liftIO (Z.feedInflate inf bs)
       fromPopper popper
    bs <- liftIO $ Z.finishInflate inf
    unless (B.null bs) (yield bs)
    return r
{-# INLINABLE decompress #-}


-- | Compress bytes flowing from a 'Producer'.
--
-- See the "Codec.Compression.Zlib" module for details about
-- 'ZC.CompressionLevel' and 'ZC.WindowBits'.
--
-- @
-- 'compress' :: 'MonadIO' m
--          => 'ZC.CompressionLevel'
--          -> 'ZC.WindowBits'
--          -> 'Producer' 'B.ByteString' m r
--          -> 'Producer' 'B.ByteString' m r
-- @
compress
  :: MonadIO m
  => ZC.CompressionLevel
  -> ZC.WindowBits
  -> Proxy x' x () B.ByteString m r -- ^ Decompressed stream
  -> Proxy x' x () B.ByteString m r -- ^ Compressed stream
compress clevel wbits p0 = do
    def <- liftIO $ Z.initDeflate (fromCompressionLevel clevel) wbits
    r <- for p0 $ \bs -> do
       popper <- liftIO (Z.feedDeflate def bs)
       fromPopper popper
    fromPopper $ Z.finishDeflate def
    return r
{-# INLINABLE compress #-}

--------------------------------------------------------------------------------

-- $ccz-re-export
--
-- The following are re-exported from "Codec.Compression.Zlib" for your
-- convenience.

--------------------------------------------------------------------------------
-- Internal stuff

-- | Produce values from the given 'Z.Popper' until exhausted.
fromPopper :: MonadIO m => Z.Popper -> Producer' B.ByteString m ()
fromPopper pop = loop where
    loop = do
      mbs <- liftIO pop
      case mbs of
         Nothing -> return ()
         Just bs -> yield bs >> loop
{-# INLINABLE fromPopper #-}

-- We need this function until the @zlib@ library hides the
-- 'ZC.CompressionLevel' constructors in future version 0.7.
fromCompressionLevel :: ZC.CompressionLevel -> Int
fromCompressionLevel level = case level of
    ZC.DefaultCompression   -> -1
    ZC.NoCompression        -> 0
    ZC.BestSpeed            -> 1
    ZC.BestCompression      -> 9
    ZC.CompressionLevel n
         | n >= 0 && n <= 9 -> fromIntegral n
    _  -> error "CompressLevel must be in the range 1..9"
