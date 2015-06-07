with (import <nixpkgs> {}).pkgs;
let
    ghc = haskellPackages.ghcWithPackages (pkgs:
        with pkgs; [
            network
            mtl
            transformers
            stm

            # Tests
            tasty
            QuickCheck
            tastyQuickcheck
            HUnit
            tastyHunit
        ]
    );
in
    stdenv.mkDerivation {
        name = "kademlia-env";
        buildInputs = [ ghc haskellPackages.cabalInstall ];
        shellHook = "eval $(grep export ${ghc}/bin/ghc)";
    }
