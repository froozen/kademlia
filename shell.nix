{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc7101" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, bytestring, containers, HUnit, mtl
      , network, QuickCheck, stdenv, stm, tasty, tasty-hunit
      , tasty-quickcheck, transformers, transformers-compat
      , cabal-install
      }:
      mkDerivation {
        pname = "kademlia";
        version = "1.1.0.0";
        src = ./.;
        buildDepends = [
          base bytestring containers mtl network stm transformers
          transformers-compat
          cabal-install
        ];
        testDepends = [
          base bytestring containers HUnit mtl network QuickCheck stm tasty
          tasty-hunit tasty-quickcheck transformers transformers-compat
        ];
        homepage = "https://github.com/froozen/kademlia";
        description = "An implementation of the Kademlia DHT Protocol";
        license = stdenv.lib.licenses.bsd3;
      };

  drv = pkgs.haskell.packages.${compiler}.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
