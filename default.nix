let
  # Pin the Miso repository to a particular commit, import the expression, and
  # retrieve the nixpkgs package set it provides.
  #
  # Last updated: 4 September 2020
  pkgs = (
    import (
      builtins.fetchTarball {
        url = "https://github.com/dmjio/miso/archive/db5400ad7801076a8eac1c071a654b7317b78811.tar.gz";
        sha256 = "0ij4gw8ypnrdh7klscqczzycyhdnwzdcp83i9pxdbd8y9kmcgz4l";
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
          servant-jsaddle =
            let
              src =
                pkgs.fetchFromGitHub {
                  owner = "haskell-servant";
                  repo = "servant-jsaddle";
                  rev = "2ccf13d185e26d4cb4a51622e748ec64336435f4";
                  sha256 = "066vr1rfq6bjn3xx9g52z2vgp1ibyz50z3hzwaqq3fzxnr2srpjs";
                };
            in
              pkgs.haskell.lib.doJailbreak (
                hself.callCabal2nix "servant-jsaddle" src {}
              );
          validation-selective =
           let
             src =
               pkgs.fetchFromGitHub {
                 owner = "kowainik";
                 repo =  "validation-selective";
                 rev =  "016378ba9bd8e459bd6599e58dfbdd395017d58d";
                 sha256 =  "1mb74aw287cmwnz91m36c9fz89p5gf4ks9qw6g658jl3nfn326g1";
               };
          in
            pkgs.haskell.lib.doJailbreak (
              hself.callCabal2nix "validation-selective" src {}
            );
        }
      );
    }
  );
in
  with pkgs;
  haskellPackages.callCabal2nix "app" ./. {
    servant-jsaddle = haskellPackages.servant-jsaddle;
  }
