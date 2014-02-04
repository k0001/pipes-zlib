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
import           Data.Foldable             (forM_)
import           Pipes
import qualified Data.ByteString           as B

--------------------------------------------------------------------------------

-- | Decompress bytes flowing downstream.
--
-- See the "Codec.Compression.Zlib" module for details about 'Z.WindowBits'.
decompress
  :: MonadIO m
  => ZC.WindowBits
  -> Lens' (Producer B.ByteString m r)
           (Producer B.ByteString m r)
decompress config k p0 = k $ do
      inf <- liftIO $ Z.initInflate config
      go p0 inf
  where
    go p inf = do
      ebs <- lift $ next p
      case ebs of
        Left   r        -> do
          bs <- liftIO $ Z.finishInflate inf
          unless (B.null bs) $ yield bs
          return r
        Right (bs, p') -> do
          popper <- liftIO $ Z.feedInflate inf bs
          fromPopper popper
          go p' inf
{-# INLINABLE decompress #-}

-- | Compress bytes flowing downstream.
--
-- See the "Codec.Compression.Zlib" module for details about
-- 'ZC.CompressionLevel' and 'ZC.WindowBits'.
compress
  :: MonadIO m
  => ZC.CompressionLevel
  -> ZC.WindowBits
  -> Lens' (Producer B.ByteString m r)
           (Producer B.ByteString m r)
compress level config k p0 = k $ do
      def <- liftIO $ Z.initDeflate level' config
      go p0 def
  where
    level' = fromCompressionLevel level
    go p def = do
      ebs <- lift $ next p
      case ebs of
        Left   r        -> do
          mbs <- liftIO $ Z.finishDeflate def
          forM_ mbs yield
          return r
        Right (bs, p') -> do
          popper <- liftIO $ Z.feedDeflate def bs
          fromPopper popper
          go p' def
{-# INLINABLE compress #-}

--------------------------------------------------------------------------------

-- $ccz-re-export
--
-- The following are re-exported from "Codec.Compression.Zlib" for your
-- convenience.

--------------------------------------------------------------------------------
-- Internal stuff
type Lens' a b = forall f . Functor f => (b -> f b) -> (a -> f a)

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
