{-# LANGUAGE OverloadedStrings #-}
import Control.Arrow
import qualified Data.ByteString.Char8 as B8
import Data.List
import Data.Monoid ((<>))
import Data.Ord
import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.QuickCheck.Instances ()
import Test.Tasty.HUnit

import qualified Pipes as P
import qualified Pipes.Prelude as P
import qualified Pipes.Zlib as PZ
import qualified Pipes.GZip as PGZ

main = defaultMain tests

--------------------------------------------------------------------------------
tests :: TestTree
tests = testGroup "Tests" [properties, unitTests]

properties :: TestTree
properties = testGroup "Properties" [qcProps]

qcProps = testGroup "(checked by QuickCheck)"
 [ QC.testProperty "id == decompress . compress" $ \bs cl -> QC.ioProperty $ do
     let pc = PZ.compress cl PZ.defaultWindowBits (P.yield bs)
         pd = PZ.decompress PZ.defaultWindowBits pc
     bs' <- B8.concat <$> P.toListM pd
     return (bs QC.=== bs')

 , QC.testProperty "id == decompress' . compress" $ \bs bsl cl -> QC.ioProperty $ do
     let pc = PZ.compress cl PZ.defaultWindowBits (P.yield bs)
         pd = PZ.decompress' PZ.defaultWindowBits (pc >> P.yield bsl)
     (bs', elr) <- first B8.concat <$> P.toListM' pd
     case elr of
        Left pl -> do
           bsl' <- B8.concat <$> P.toListM pl
           return $ (bs QC.=== bs') QC..&&. (bsl QC.=== bsl')
        Right () -> do
           return $ (bs QC.=== bs') QC..&&. (bsl QC.=== B8.empty)
 ]

unitTests = testGroup "Unit tests"
  [ testCase "Zlib compression" $ do
      let pc = PZ.compress PZ.defaultCompression PZ.defaultWindowBits
                   (P.yield bsUncompressed)
      bs <- B8.concat <$> P.toListM pc
      bs @?= bsCompressedZlibDefault
  , testCase "Zlib decompression" $ do
      let pd = PZ.decompress PZ.defaultWindowBits (P.yield bsCompressedZlibDefault)
      bs <- B8.concat <$> P.toListM pd
      bs @?= bsUncompressed
  , testCase "GZip compression" $ do
      let pd = PGZ.compress PGZ.defaultCompression (P.yield bsUncompressed)
      bs <- B8.concat <$> P.toListM pd
      bs @?= bsCompressedGZipDefault
  , testCase "GZip decompression" $ do
      let pd = PGZ.decompress (P.yield bsCompressedGZipDefault)
      bs <- B8.concat <$> P.toListM pd
      bs @?= bsUncompressed
  , testCase "Concatenated GZip decompression" $ do
      let pd = PGZ.decompress $ do
                 P.yield bsCompressedGZipDefault
                 P.yield bsCompressedGZipDefault
      bs <- B8.concat <$> P.toListM pd
      bs @?= (bsUncompressed <> bsUncompressed)
  , testCase "Concatenated GZip decompression with leftovers" $ do
      let bsl = "xxxxx"
          pd = PGZ.decompress' $ do
                 P.yield bsCompressedGZipDefault
                 P.yield bsCompressedGZipDefault
                 P.yield bsl
      (bs, elr) <- first B8.concat <$> P.toListM' pd
      bs @?= (bsUncompressed <> bsUncompressed)
      case elr of
         Right () -> error "unexpected"
         Left pl -> do
            bsl' <- B8.concat <$> P.toListM pl
            bsl' @?= bsl
  ]

bsUncompressed :: B8.ByteString
bsUncompressed = "foo"

bsCompressedZlibDefault :: B8.ByteString
bsCompressedZlibDefault = "x\156K\203\207\a\NUL\STX\130\SOHE"

bsCompressedGZipDefault :: B8.ByteString
bsCompressedGZipDefault =
  "\US\139\b\NUL\NUL\NUL\NUL\NUL\NUL\ETXK\203\207\a\NUL!es\140\ETX\NUL\NUL\NUL"

instance QC.Arbitrary PZ.CompressionLevel where
  arbitrary = PZ.compressionLevel <$> fmap (flip mod 10) QC.arbitrary

