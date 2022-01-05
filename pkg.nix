{ mkDerivation, base, bytestring, HUnit, lib, pipes, QuickCheck
, quickcheck-instances, streaming-commons, tasty, tasty-hunit
, tasty-quickcheck, transformers
}:
mkDerivation {
  pname = "pipes-zlib";
  version = "0.5";
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
  license = lib.licenses.bsd3;
}
