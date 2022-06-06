{ mkDerivation, aeson, aeson-casing, base, bytestring, file-embed
, hspec, hspec-discover, hspec-megaparsec, lib, megaparsec
, scientific, text, unordered-containers, vector
}:
mkDerivation {
  pname = "jsonpath";
  version = "0.2.1.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson base megaparsec scientific text unordered-containers vector
  ];
  testHaskellDepends = [
    aeson aeson-casing base bytestring file-embed hspec
    hspec-megaparsec megaparsec text unordered-containers vector
  ];
  testToolDepends = [ hspec-discover ];
  homepage = "https://github.com/akshaymankar/jsonpath-hs#readme";
  description = "Library to parse and execute JSONPath";
  license = lib.licenses.bsd3;
}
