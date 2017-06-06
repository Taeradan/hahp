{ mkDerivation, aeson, aeson-pretty, base, bytestring, containers
, hmatrix, liblapack, openblasCompat, parallel, random, stdenv
, tasty, tasty-hunit, tasty-quickcheck, tasty-smallcheck, time
}:
mkDerivation {
  pname = "hahp";
  version = "0.1.3.1";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base containers hmatrix parallel random time
  ];
  librarySystemDepends = [ liblapack openblasCompat ];
  executableHaskellDepends = [
    aeson aeson-pretty base bytestring time
  ];
  testHaskellDepends = [
    base containers hmatrix tasty tasty-hunit tasty-quickcheck
    tasty-smallcheck
  ];
  description = "Analytic Hierarchy Process";
  license = stdenv.lib.licenses.agpl3;
}
