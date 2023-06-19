{
  description = "stepper";
  outputs =
    { self, nixpkgs }:
    let
      system = "x86_64-linux";
      ghc = "ghc92";
      pkgs = nixpkgs.legacyPackages.${system};
      haskellPackages =
        pkgs.haskell.packages.${ghc}.extend(hself: hsuper: {
          stepper = haskellPackages.callCabal2nix "stepper" "${self}/src/" {};
        });
    in
    {
      packages.${system}.stepper = haskellPackages.stepper;
      defaultPackage.${system} = self.packages.${system}.stepper;
      devShell.${system} = pkgs.mkShell {
        buildInputs = [
          (haskellPackages.ghcWithPackages(p: p.stepper.getCabalDeps.executableHaskellDepends))
          haskellPackages.stepper.getCabalDeps.executableToolDepends
          haskellPackages.hie-bios
          haskellPackages.haskell-language-server
          haskellPackages.cabal-install
        ];
      };
    };
}
