let
  pkgs = import ./nixpkgs.nix { overlays = import ./overlays.nix; };
in
with pkgs;

{ inherit (haskellPackages) optparse-repline;
  inherit optparse-repline-shell;
}
