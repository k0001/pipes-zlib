{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | This module exports utilities to compress and decompress @pipes@ streams
-- using the zlib compression codec.
--
-- If you want to compress or decompress GZip streams, use the "Pipes.GZip"
-- module instead.

module Pipes.Zlib (
  -- * Streams
    decompress
  , decompress'
  , compress

  -- * Compression levels
  , CompressionLevel
  , defaultCompression
  , noCompression
  , bestSpeed
  , bestCompression
  , compressionLevel

  -- * Window size
  -- $ccz-re-export
  , Z.defaultWindowBits
  , windowBits

  -- * Exceptions
  , Err_Decompress(..)
  ) where

import Data.Function (fix)
import Data.Streaming.Zlib as Z
import Control.Exception (Exception, throwIO)
import Control.Monad (unless)
import qualified Data.ByteString as B
import Pipes

--------------------------------------------------------------------------------

data Err_Decompress = UnexpectedEndOfInput | UnexpectedLeftovers deriving (Eq, Show)
instance Exception Err_Decompress

-- | Decompress bytes flowing from a 'Producer'.
--
-- See the "Codec.Compression.Zlib" module for details about 'Z.WindowBits'.
--
-- Throws 'UnexpectedEndOfInput' if the compressed stream ends prematurely.
-- Throws 'UnexpectedLeftovers' if the compressed stream is not completely
-- consumed by decompression.
decompress
  :: MonadIO m
  => Z.WindowBits
  -> Producer B.ByteString m r -- ^ Compressed stream
  -> Producer' B.ByteString m r -- ^ Decompressed stream
decompress wbits p = decompress' wbits p >>= \case
  Left _ -> liftIO (throwIO UnexpectedLeftovers)
  Right r -> pure r
{-# INLINABLE decompress #-}

-- | Decompress bytes flowing from a 'Producer', returning any leftover input
-- that follows the compressed stream.
--
-- See the "Codec.Compression.Zlib" module for details about 'Z.WindowBits'.
--
-- Throws 'UnexpectedEndOfInput' if the compressed stream ends prematurely.
decompress'
  :: forall m r
   . MonadIO m
  => Z.WindowBits
  -> Producer B.ByteString m r -- ^ Compressed stream
  -> Producer' B.ByteString m (Either (Producer B.ByteString m r) r)
  -- ^ Decompressed stream, ending with either leftovers or a result
decompress' wbits p0 = do
    inf <- liftIO (Z.initInflate wbits)
    res <- go inf p0
    bs <- liftIO (Z.finishInflate inf)
    unless (B.null bs) (yield bs)
    liftIO (Z.isCompleteInflate inf) >>= \case
       True -> pure ()
       False -> liftIO (throwIO UnexpectedEndOfInput)
    pure res
  where
    go :: Inflate
       -> Producer B.ByteString m b
       -> Producer' B.ByteString m (Either (Producer B.ByteString m b) b)
    go inf = fix $ \k p -> do
      lift (next p) >>= \case
         Left r -> pure (Right r)
         Right (bs, p') -> do
            fromPopper =<< liftIO (Z.feedInflate inf bs)
            leftover <- liftIO (Z.getUnusedInflate inf)
            if B.null leftover
               then k p'
               else pure (Left (yield leftover >> p'))
{-# INLINABLE decompress' #-}

-- | Compress bytes flowing from a 'Producer'.
--
-- See the "Codec.Compression.Zlib" module for details about
-- 'Z.CompressionLevel' and 'Z.WindowBits'.
compress
  :: forall m r
   . MonadIO m
  => CompressionLevel
  -> Z.WindowBits
  -> Producer B.ByteString m r -- ^ Decompressed stream
  -> Producer' B.ByteString m r -- ^ Compressed stream
compress (CompressionLevel clevel) wbits p0 = do
    def <- liftIO (Z.initDeflate clevel wbits)
    res <- go def p0
    fromPopper (Z.finishDeflate def)
    pure res
  where
    go :: Deflate -> Producer B.ByteString m b -> Producer' B.ByteString m b
    go def = fix $ \k p -> do
      lift (next p) >>= \case
         Left r -> pure r
         Right (bs, p') -> do
            fromPopper =<< liftIO (Z.feedDeflate def bs)
            k p'
{-# INLINABLE compress #-}

--------------------------------------------------------------------------------

-- $ccz-re-export
--
-- The following are re-exported from "Codec.Compression.Zlib" for your
-- convenience.

--------------------------------------------------------------------------------
-- Compression Levels

-- | How hard should we try to compress?
newtype CompressionLevel = CompressionLevel Int
  deriving (Show, Read, Eq, Ord)

defaultCompression, noCompression, bestSpeed, bestCompression :: CompressionLevel
defaultCompression = CompressionLevel (-1)
noCompression      = CompressionLevel 0
bestSpeed          = CompressionLevel 1
bestCompression    = CompressionLevel 9

-- | A specific compression level between 0 and 9.
compressionLevel :: Int -> CompressionLevel
compressionLevel n
  | n >= 0 && n <= 9 = CompressionLevel n
  | otherwise        = error "CompressionLevel must be in the range 0..9"

windowBits :: Int -> WindowBits
windowBits = WindowBits

--------------------------------------------------------------------------------
-- Internal stuff

-- | Produce values from the given 'Z.Popper' until exhausted.
fromPopper :: MonadIO m => Z.Popper -> Proxy x' x () B.ByteString m ()
fromPopper = \pop -> fix $ \k -> do
   liftIO pop >>= \case
      PRDone -> pure ()
      PRError e -> liftIO (throwIO e)
      PRNext bs -> yield bs >> k
{-# INLINABLE fromPopper #-}
