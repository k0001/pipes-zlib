-- | This module exports utilities to compress and decompress @pipes@ streams
-- using the zlib compression codec.

module Control.Proxy.Zlib (
  -- * Streams
    decompressD
  , compressD

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
import           Control.Monad             (forever, unless)
import           Control.Monad.Trans.Class (lift)
import           Control.Proxy             ((>->))
import qualified Control.Proxy             as P
import qualified Data.ByteString           as B
import           Data.Traversable          (mapM)
import           Prelude                   hiding (mapM)

-- | Decompress bytes flowing downstream using the given 'Z.WindowBits'.
--
-- See the "Codec.Compression.Zlib" module for details about these values.
decompressD
  :: P.Proxy p
  => Z.WindowBits
  -> () -> P.Pipe p B.ByteString B.ByteString IO r
decompressD config () = P.runIdentityP . forever $ do
    inf <- lift (Z.initInflate config)
    popper <- lift . Z.feedInflate inf =<< P.request ()
    (P.unitD >-> fromPopperS popper) ()
    bs <- lift (Z.finishInflate inf)
    unless (B.null bs) $ P.respond bs

-- | Compress bytes flowing downstream.
--
-- See the "Codec.Compression.Zlib" module for details about these values.
compressD
  :: P.Proxy p
  => ZC.CompressionLevel
  -> Z.WindowBits
  -> () -> P.Pipe p B.ByteString B.ByteString IO r
compressD level config () = P.runIdentityP loop where
    loop = forever $ do
        def <- lift (Z.initDeflate level' config)
        popper <- lift . Z.feedDeflate def =<< P.request ()
        (P.unitD >-> fromPopperS popper) ()
        mapM P.respond =<< lift (Z.finishDeflate def)
    level' = fromCompressionLevel level


--------------------------------------------------------------------------------

-- $ccz-re-export
--
-- The following are re-exported from "Codec.Compression.Zlib" for your
-- convenience.

--------------------------------------------------------------------------------
-- Internal stuff

-- | Produce values from the given 'Z.Poppler' until exhausted.
fromPopperS :: P.Proxy p => Z.Popper -> () -> P.Producer p B.ByteString IO ()
fromPopperS pop () = P.runIdentityP loop where
    loop = do
        mbs <- lift pop
        case mbs of
          Nothing -> return ()
          Just bs -> P.respond bs >> loop

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

