let
  # Pin the Miso repository to a particular commit, import the expression, and
  # retrieve the nixpkgs package set it provides.
  #
  # Last updated: 4 September 2020
  pkgs = (
    import (
      builtins.fetchTarball {
        url = "https://github.com/dmjio/miso/archive/ea25964565074e73d4052b56b60b6e101fa08bc5.tar.gz";
        sha256 = "1yb9yvc0ln4yn1jk2k5kwwa1s32310abawz40yd8cqqkm1z7w6wg";
      }
    ) {}
  ).pkgs;

  # Construct a complete Haskell package set by overlaying the base package set
  # from nixpkgs with various packages from external sources.
  haskellPackages = pkgs.haskell.packages.ghcjs.override (
    oldArgs: {
      # Ensure that we have an up-to-date Hackage snapshot.
      #
      # Last updated: 4 September 2020.
      all-cabal-hashes = pkgs.fetchurl {
        url = "https://github.com/commercialhaskell/all-cabal-hashes/archive/117622c10bf41f70548af023366ad82eab9835e3.tar.gz";
        sha256 = "15zpi7x1iqkjk4dscm0z9xxshl58nmdi3sxgn8w3x86wbz03k1wv";
      };

      # Override/extend the base package set.
      overrides = pkgs.lib.composeExtensions (oldArgs.overrides or (_: _: {})) (
        hself: hsuper: {
          miso = pkgs.haskell.lib.dontCheck (
            hself.callHackage "miso" "1.7.1.0" {}
          );
          servant = pkgs.haskell.lib.dontCheck (
            hself.callHackage "servant" "0.16" {}
          );
          servant-client-core = pkgs.haskell.lib.dontCheck (
            hself.callHackage "servant-client-core" "0.16" {}
          );
          servant-client-ghcjs =
            let
              src =
                pkgs.fetchFromGitHub {
                  owner = "haskell-servant";
                  repo = "servant";
                  rev = "v0.16";
                  sha256 = "0dyn50gidzbgyq9yvqijnysai9hwd3srqvk8f8rykh09l375xb9j";
                } + "/servant-client-ghcjs";
            in
              pkgs.haskell.lib.doJailbreak (
                hself.callCabal2nix "servant-client-ghcjs" src {}
              );
        }
      );
    }
  );
in
haskellPackages.callCabal2nix "app" ./. { servant-client-ghcjs = haskellPackages.servant-client-ghcjs; }
