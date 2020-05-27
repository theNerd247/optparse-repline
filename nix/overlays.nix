let
  mkHaskellShell = name: self: super:
    { "${name}-shell" = super.haskellPackages."${name}".env.overrideAttrs 
      (old:
        { buildInputs = with super; old.buildInputs ++ [ ghcid cabal-install ];
        }
      );
    };
in
[ (import ./haskell.nix)
  (mkHaskellShell "optparse-repline")
] 
