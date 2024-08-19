{
  description = "stepper";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs =
    { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
    let
      ghc = "ghc96";
      pkgs = nixpkgs.legacyPackages.${system};

      /* workaround for https://github.com/NixOS/nixpkgs/issues/41340 */
      fixGtkDeps = pkg:
        if pkgs.stdenv.isLinux then
          pkg.overrideAttrs(oldAttrs: {strictDeps = true; buildInputs = [
            pkgs.graphene pkgs.gdk-pixbuf pkgs.gtk4 pkgs.pcre pkgs.pcre2
            pkgs.util-linux.dev pkgs.libselinux pkgs.libsepol pkgs.fribidi
            pkgs.libthai pkgs.libdatrie pkgs.xorg.libXdmcp pkgs.expat
          ];})
        else pkg;

      haskellPackages =
        pkgs.haskell.packages.${ghc}.extend(hself: hsuper: {
          gi-gdk = fixGtkDeps (pkgs.haskell.lib.dontCheck (hself.callHackage "gi-gdk" "4.0.7" {}));
          gi-gsk = fixGtkDeps (pkgs.haskell.lib.dontCheck (hself.callHackage "gi-gsk" "4.0.7" {}));
          gi-gtk = fixGtkDeps (pkgs.haskell.lib.dontCheck (hself.callHackage "gi-gtk" "4.0.8" {}));
          stepper = haskellPackages.callCabal2nix "stepper" "${self}/src/" {};
        });
    in
    {
      packages = {
        stepper = haskellPackages.stepper;
        default = haskellPackages.stepper;
      };
      devShells.default = pkgs.mkShell {
        buildInputs = [
          (haskellPackages.ghcWithPackages(p: p.stepper.getCabalDeps.executableHaskellDepends))
          haskellPackages.stepper.getCabalDeps.executableToolDepends
          haskellPackages.hie-bios
          haskellPackages.haskell-language-server
          haskellPackages.cabal-install
        ];
      };
    });
}
