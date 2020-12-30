let
  pkgs = import <nixos> {};
  unstablePkgs = import <nixos-unstable> {};
  haskellPackages = pkgs.haskellPackages;

in unstablePkgs.haskellPackages.developPackage {
  root = ./.;
  modifier = drv:
    pkgs.haskell.lib.overrideCabal drv (attrs: {
      buildTools = (attrs.buildTools or [ ]) ++ [
        haskellPackages.cabal-install
        unstablePkgs.haskellPackages.haskell-language-server
        haskellPackages.brittany
        pkgs.hlint
      ];
    });
}
