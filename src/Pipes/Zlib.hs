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
import           Control.Monad             (forever)
import           Pipes
import qualified Data.ByteString           as B

--------------------------------------------------------------------------------

-- | Decompress bytes flowing downstream.
--
-- See the "Codec.Compression.Zlib" module for details about 'Z.WindowBits'.
decompress :: MonadIO m => ZC.WindowBits -> Pipe B.ByteString B.ByteString m r
decompress config = forever $ do
    inf <- liftIO (Z.initInflate config)
    a <- awaitNonEmpty
    popper <- liftIO (Z.feedInflate inf a)
    fromPopper popper
    bs <- liftIO (Z.finishInflate inf)
    if B.null bs
        then yield bs
        else return ()
{-# INLINABLE decompress #-}

-- | Compress bytes flowing downstream.
--
-- See the "Codec.Compression.Zlib" module for details about
-- 'ZC.CompressionLevel' and 'ZC.WindowBits'.
compress
  :: MonadIO m
  => ZC.CompressionLevel -> ZC.WindowBits -> Pipe B.ByteString B.ByteString m r
compress level config = forever $ do
    def <- liftIO (Z.initDeflate level' config)
    a <- awaitNonEmpty
    popper <- liftIO (Z.feedDeflate def a)
    fromPopper popper
    mbs <- liftIO (Z.finishDeflate def)
    case mbs of
        Just bs -> yield bs
        Nothing -> return ()
  where
    level' = fromCompressionLevel level
{-# INLINABLE compress #-}

--------------------------------------------------------------------------------

-- $ccz-re-export
--
-- The following are re-exported from "Codec.Compression.Zlib" for your
-- convenience.

--------------------------------------------------------------------------------
-- Internal stuff

awaitNonEmpty :: Monad m => Consumer' B.ByteString m B.ByteString
awaitNonEmpty = loop
  where
    loop = do
        bs <- await
        if B.null bs
            then loop
            else return bs
{-# INLINABLE awaitNonEmpty #-}

-- | Produce values from the given 'Z.Poppler' until exhausted.
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

