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
import           Data.Traversable          (mapM)
import           Prelude                   hiding (mapM)

--------------------------------------------------------------------------------

-- | Decompress bytes flowing downstream.
--
-- See the "Codec.Compression.Zlib" module for details about 'Z.WindowBits'.
decompress :: ZC.WindowBits -> () -> Pipe B.ByteString B.ByteString IO r
decompress config () = forever $ do
    inf <- lift $ Z.initInflate config
    popper <- lift . Z.feedInflate inf =<< requestNonEmpty
    fromPopper popper
    bs <- lift $ Z.finishInflate inf
    unless (B.null bs) $ respond bs

-- | Compress bytes flowing downstream.
--
-- See the "Codec.Compression.Zlib" module for details about
-- 'ZC.CompressionLevel' and 'ZC.WindowBits'.
compress
  :: ZC.CompressionLevel
  -> ZC.WindowBits
  -> () -> Pipe B.ByteString B.ByteString IO r
compress level config () = forever $ do
    def <- lift $ Z.initDeflate (fromCompressionLevel level) config
    popper <- lift . Z.feedDeflate def =<< requestNonEmpty
    fromPopper popper
    mapM respond =<< lift (Z.finishDeflate def)

--------------------------------------------------------------------------------

-- $ccz-re-export
--
-- The following are re-exported from "Codec.Compression.Zlib" for your
-- convenience.

--------------------------------------------------------------------------------
-- Internal stuff

requestNonEmpty :: Monad m => Consumer B.ByteString m B.ByteString
requestNonEmpty = loop
  where
    loop = do
        bs <- request ()
        if B.null bs
            then loop
            else return bs
{-# INLINE requestNonEmpty #-}

-- | Produce values from the given 'Z.Poppler' until exhausted.
fromPopper :: Z.Popper -> Producer B.ByteString IO ()
fromPopper pop = loop
  where
    loop = do
        mbs <- lift pop
        case mbs of
            Nothing -> return ()
            Just bs -> respond bs >> loop

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

