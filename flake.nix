{
  description = "A very basic flake";

  inputs = {
    nixpkgs = { url = "github:NixOS/nixpkgs/nixos-unstable"; };
    flake-utils.url = "github:numtide/flake-utils";
    easy-hls = {
      url = "github:jkachmar/easy-hls-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, flake-utils, easy-hls }:
    flake-utils.lib.eachSystem ["x86_64-linux" "x86_64-darwin"] ( system:
      let
        pkgs = import nixpkgs { inherit system; };
        hp = pkgs.haskellPackages.extend (self: super: {
          ics-print = self.callPackage ./. {};
        });
        hls = easy-hls.defaultPackage.${system};
      in
      rec {

        packages.ics-print = pkgs.haskell.lib.justStaticExecutables hp.ics-print;

        defaultPackage = packages.ics-print;

        apps.ics-print = flake-utils.lib.mkApp { drv = packages.ics-print; };
        defaultApp = apps.ics-print;

        devShell = hp.shellFor {
          packages = h: [h.ics-print];
          withHoogle = true;
          buildInputs = with pkgs; [
            entr
            cabal-install
            hp.hlint
            stylish-haskell
            ghcid
            hls
          ];
        };
      }
    );
}
