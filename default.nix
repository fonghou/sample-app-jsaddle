let
  # Pin the Miso repository to a particular commit, import the expression, and
  # retrieve the nixpkgs package set it provides.
  #
  # Last updated: 4 September 2020
  pkgs = (
    import (
      builtins.fetchTarball {
        url = "https://github.com/dmjio/miso/archive/d089cb6b72ba42a79d8be6eeb31eda6a2ad7ef36.tar.gz";
        sha256 = "0c6sx43znxmf7kcpn8s59x02rr7a4yilj1xfjjw1l5srf9dzxix5";
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
        }
      );
    }
  );
in
  with pkgs;
  haskellPackages.callCabal2nix "app" ./. {
    servant-jsaddle = haskellPackages.servant-jsaddle;
  }
