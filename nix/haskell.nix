self: super:

{ haskellPackages = super.haskellPackages.override
  (old:
    { overrides = super.lib.composeExtensions (old.overrides or (_: _: {}))
      ( hself: hsuper:
        { haskeline =
            super.haskell.lib.dontCheck (
              hself.callCabal2nix "haskeline" (builtins.fetchTarball {
                url = "https://hackage.haskell.org/package/haskeline-0.8.1.0/haskeline-0.8.1.0.tar.gz";
                sha256 = "sha256:1r33spdza6mj9qfrsv08sysnkvdz7l8qdhwa5yhq2f9my1ikg85x";
                }) {}
            );

          optparse-repline = hself.callCabal2nix "optparse-repline" ../. {};

          repline = hself.callCabal2nix "repline" (builtins.fetchTarball {
            url = "https://hackage.haskell.org/package/repline-0.4.0.0/repline-0.4.0.0.tar.gz";
            sha256 = "sha256:1d2gfr8a2psxv3gl6xacn861naxrhrqa1msqf0zndhby4ysp0245";
            }) {};
        }
      );
    }
  );
}
