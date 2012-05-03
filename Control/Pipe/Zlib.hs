module Control.Pipe.Zlib (
  gzip,
  gunzip,
  decompress,
  compress
  ) where

import Codec.Zlib
import Control.Monad
import Control.Monad.IO.Class
import Control.Pipe
import Control.Pipe.Combinators
import qualified Data.ByteString as B
import Prelude hiding (catch)

-- | Gzip compression with default parameters.
gzip :: MonadIO m => Pipe B.ByteString B.ByteString m ()
gzip = compress 1 (WindowBits 31)

-- | Gzip decompression with default parameters.
gunzip :: MonadIO m => Pipe B.ByteString B.ByteString m ()
gunzip = decompress (WindowBits 31)

decompress
    :: MonadIO m
    => WindowBits
    -> Pipe B.ByteString B.ByteString m ()
decompress config = do
    inf <- liftIO $ initInflate config
    forP $ \x -> do
      popper <- liftIO $ feedInflate inf x
      yieldPopper popper

    chunk <- liftIO $ finishInflate inf
    unless (B.null chunk) $ yield chunk

compress
    :: MonadIO m
    => Int
    -> WindowBits
    -> Pipe B.ByteString B.ByteString m ()
compress level config = do
    def <- liftIO $ initDeflate level config
    forP $ \x -> do
      popper <- liftIO $ feedDeflate def x
      yieldPopper popper

    let popper = liftIO $ finishDeflate def
    yieldPopper popper

yieldPopper :: MonadIO m => Popper -> Pipe a B.ByteString m ()
yieldPopper pop = do
  x <- liftIO pop
  case x of
    Nothing -> return ()
    Just chunk -> yield chunk >> yieldPopper pop
