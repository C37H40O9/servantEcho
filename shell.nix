{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc865", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, aeson, async, base, bytestring, clock
      , containers, directory, extra, filepath, free, freer, gauge, lens-aeson, logging
      , microlens, mtl, network, newtype-generics, process, random
      , safe-exceptions, servant, servant-client, servant-server, stdenv
      , stm, text, time, transformers, transformers-either
      , unordered-containers, uuid, vector, warp, yaml, wai
      , base-compat , lens , http-client , http-types , hspec-wai
      , swagger2, servant-swagger, servant-docs, natural-transformation
      }:
      mkDerivation {
        pname = "servantEcho";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = false;
        isExecutable = true;
        libraryHaskellDepends = [
          aeson async base bytestring clock containers directory extra
          filepath free freer gauge lens-aeson logging microlens mtl network
          newtype-generics process random safe-exceptions servant
          servant-client servant-server stm text time transformers
          transformers-either unordered-containers uuid vector warp yaml
          wai base-compat lens http-client http-types hspec-wai swagger2
          servant-swagger servant-docs natural-transformation
        ];
        executableHaskellDepends = [ base ];
        doHaddock = false;
        doCheck = false;
        description = "Test servant app";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
