{ mkDerivation, aeson, async, base, bytestring, clock, containers
, directory, extra, filepath, free, gauge, lens-aeson, logging, microlens
, mtl, network, newtype-generics, process, random, safe-exceptions
, servant, servant-client, servant-server, stdenv, stm, text, time
, transformers, transformers-either, unordered-containers, uuid
, vector, warp, yaml, wai, base-compat, lens, http-client, http-types
, hspec-wai, swagger2, servant-swagger
}:
mkDerivation {
  pname = "servantEcho";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson async base bytestring clock containers directory extra
    filepath free gauge lens-aeson logging microlens mtl network
    newtype-generics process random safe-exceptions servant
    servant-client servant-server stm text time transformers
    transformers-either unordered-containers uuid vector warp yaml
    wai base-compat lens http-client http-types hspec-wai swagger2
    servant-swagger
  ];
  executableHaskellDepends = [ base ];
  doHaddock = false;
  doCheck = false;
  description = "Test servant app";
  license = stdenv.lib.licenses.bsd3;
}
