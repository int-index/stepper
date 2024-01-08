{
  description = "stepper";
  outputs =
    { self, nixpkgs }:
    let
      system = "x86_64-linux";
      ghc = "ghc96";
      pkgs = nixpkgs.legacyPackages.${system};

      /* workaround for https://github.com/NixOS/nixpkgs/issues/41340 */
      fixGtkDeps = pkg:
        pkg.overrideAttrs(oldAttrs: {strictDeps = true; buildInputs = [
          pkgs.graphene pkgs.gdk-pixbuf pkgs.gtk4 pkgs.pcre pkgs.pcre2
          pkgs.util-linux.dev pkgs.libselinux pkgs.libsepol pkgs.fribidi
          pkgs.libthai pkgs.libdatrie pkgs.xorg.libXdmcp pkgs.expat
        ];});

      haskellPackages =
        pkgs.haskell.packages.${ghc}.extend(hself: hsuper: {
          gi-gdk = fixGtkDeps (pkgs.haskell.lib.dontCheck (hself.callHackage "gi-gdk" "4.0.7" {}));
          gi-gsk = fixGtkDeps (pkgs.haskell.lib.dontCheck (hself.callHackage "gi-gsk" "4.0.7" {}));
          gi-gtk = fixGtkDeps (pkgs.haskell.lib.dontCheck (hself.callHackage "gi-gtk" "4.0.8" {}));
          stepper = haskellPackages.callCabal2nix "stepper" "${self}/src/" {};
        });
    in
    {
      packages.${system} = {
        stepper = haskellPackages.stepper;
        default = self.packages.${system}.stepper;
      };
      devShells.${system}.default = pkgs.mkShell {
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
