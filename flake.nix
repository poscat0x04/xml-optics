{
  inputs.nixpkgs.url = github:poscat0x04/nixpkgs/dev;
  inputs.flake-utils.url = github:poscat0x04/flake-utils;

  outputs = { self, nixpkgs, flake-utils, ... }: with flake-utils;
    eachDefaultSystem (
      system:
        let
          pkgs = import nixpkgs { inherit system; overlays = [ self.overlay ]; };
        in
          with pkgs;
          {
            devShell = xml-optics-dev.envFunc { withHoogle = true; };
            defaultPackage = xml-optics;
          }
    ) // {
      overlay = self: super:
        let
          hpkgs = super.haskellPackages;
          xml-optics = hpkgs.callCabal2nix "xml-optics" ./. {};
        in
          with super; with haskell.lib;
          {
            inherit xml-optics;
            xml-optics-dev = addBuildTools xml-optics [
              haskell-language-server
              cabal-install
            ];
          };
    };
}
