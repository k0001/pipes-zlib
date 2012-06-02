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
gzip :: MonadIO m => Pipe B.ByteString B.ByteString m r
gzip = compress 1 (WindowBits 31)

-- | Gzip decompression with default parameters.
gunzip :: MonadIO m => Pipe B.ByteString B.ByteString m r
gunzip = decompress (WindowBits 31)

decompress
    :: MonadIO m
    => WindowBits
    -> Pipe B.ByteString B.ByteString m r
decompress config = do
    inf <- liftIO $ initInflate config
    let finalize = do chunk <- liftIO $ finishInflate inf
                      unless (B.null chunk) $ yield chunk
    forP' finalize $ \x -> do
      popper <- liftIO $ feedInflate inf x
      yieldPopper popper

compress
    :: MonadIO m
    => Int
    -> WindowBits
    -> Pipe B.ByteString B.ByteString m r
compress level config = do
    def <- liftIO $ initDeflate level config
    let finalize = yieldPopper (finishDeflate def)
    forP' finalize $ \x -> do
      popper <- liftIO $ feedDeflate def x
      yieldPopper popper

forP' :: Monad m
      => Pipe a b m r2
      -> (a -> Pipe a b m r1)
      -> Pipe a b m r
forP' p f = forP f >> p >> discard

yieldPopper :: MonadIO m => Popper -> Pipe a B.ByteString m ()
yieldPopper pop = do
  x <- liftIO pop
  case x of
    Nothing -> return ()
    Just chunk -> yield chunk >> yieldPopper pop
