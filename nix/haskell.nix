self: super:

{ haskellPackages = super.haskellPackages.override 
  (old:
    { overrides = super.lib.composeExtensions (old.overrides or (_: _: {}))
      ( hself: hsuper:
        { optparse-repline = hself.callCabal2nix "optparse-repline" ../. {};
        }
      );
    }
  );
}
