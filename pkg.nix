{ mkDerivation, base, bytestring, HUnit, pipes, QuickCheck
, quickcheck-instances, stdenv, streaming-commons, tasty
, tasty-hunit, tasty-quickcheck, transformers
}:
mkDerivation {
  pname = "pipes-zlib";
  version = "0.4.4.1";
  src = ./.;
  libraryHaskellDepends = [
    base bytestring pipes streaming-commons transformers
  ];
  testHaskellDepends = [
    base bytestring HUnit pipes QuickCheck quickcheck-instances tasty
    tasty-hunit tasty-quickcheck
  ];
  homepage = "https://github.com/k0001/pipes-zlib";
  description = "Zlib and GZip compression and decompression for Pipes streams";
  license = stdenv.lib.licenses.bsd3;
}
