{-# LANGUAGE RankNTypes #-}

-- | This module exports utilities to compress and decompress @pipes@ streams
-- using the zlib compression codec.

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

import qualified Codec.Zlib                as Z
import qualified Codec.Compression.Zlib    as ZC
import           Control.Monad             (unless)
import           Pipes
import qualified Data.ByteString           as B
import           Pipes.Lift                (evalStateP)
import           Pipes.Parse

--------------------------------------------------------------------------------

-- | Decompress bytes flowing downstream.
--
-- See the "Codec.Compression.Zlib" module for details about 'Z.WindowBits'.
decompress :: MonadIO m
           => ZC.WindowBits
           -> Producer B.ByteString m r
           -> Producer B.ByteString m r
decompress config producer = evalStateP producer $ decompressParser config
{-# INLINABLE decompress #-}

-- | Compress bytes flowing downstream.
--
-- See the "Codec.Compression.Zlib" module for details about
-- 'ZC.CompressionLevel' and 'ZC.WindowBits'.
compress
  :: MonadIO m
  => ZC.CompressionLevel
  -> ZC.WindowBits
  -> Producer B.ByteString m r
  -> Producer B.ByteString m r
compress level config producer = evalStateP producer $
                                   compressParser level config
{-# INLINABLE compress #-}

--------------------------------------------------------------------------------

-- $ccz-re-export
--
-- The following are re-exported from "Codec.Compression.Zlib" for your
-- convenience.

--------------------------------------------------------------------------------
-- Internal stuff

-- | Parser to decompress a 'Producer'
decompressParser
  :: MonadIO m
  => ZC.WindowBits
  -> Producer B.ByteString (StateT (Producer B.ByteString m r) m) r
decompressParser config = do
    inf <- liftIO $ Z.initInflate config
    r <- feed inf
    bs <- liftIO $ Z.finishInflate inf
    unless (B.null bs) $ yield bs
    return r
  where
    feed inf = do
      x <- lift draw
      case x of
        Left  r  -> return r
        Right bs -> do
          popper <- liftIO $ Z.feedInflate inf bs
          fromPopper popper
          feed inf
{-# INLINABLE decompressParser #-}

-- | Parser to compress a 'Producer'
compressParser
  :: MonadIO m
  => ZC.CompressionLevel
  -> ZC.WindowBits
  -> Producer B.ByteString (StateT (Producer B.ByteString m r) m) r
compressParser level config = do
    def <- liftIO $ Z.initDeflate level' config
    r <- feed def
    mbs <- liftIO $ Z.finishDeflate def
    case mbs of
      Just bs -> yield bs
      Nothing -> return ()
    return r
  where
    level' = fromCompressionLevel level
    feed def = do
      x <- lift draw
      case x of
        Left  r  -> return r
        Right bs -> do
          popper <- liftIO $ Z.feedDeflate def bs
          fromPopper popper
          feed def
{-# INLINABLE compressParser #-}

-- | Produce values from the given 'Z.Popper' until exhausted.
fromPopper :: MonadIO m => Z.Popper -> Producer' B.ByteString m ()
fromPopper pop = loop
  where
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
