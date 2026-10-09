{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
  };
  outputs =
    inputs@{ flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [
        "x86_64-linux"
        "aarch64-linux"
        "x86_64-darwin"
        "aarch64-darwin"
      ];
      imports = [
        inputs.haskell-flake.flakeModule
      ];
      perSystem =
        {
          self',
          pkgs,
          ...
        }:
        {
          haskellProjects.default = {
            settings = {
              h-raylib = {
                extraBuildDepends = with pkgs; [
                  libGL
                  libx11
                  libxinerama
                  libxcursor
                  libxrandr
                  libxi
                ];
              };
              hslua = { super, ... }: { custom = _: super.hslua_2_5_0; };
              hslua-packaging = { super, ... }: { custom = _: super.hslua-packaging_2_4_1; };
              hslua-typing = { super, ... }: { custom = _: super.hslua-typing_0_2_0; };
              hslua-objectorientation = { super, ... }: { custom = _: super.hslua-objectorientation_2_5_0; };
              maybe-game = {
                haddock = false;
              };
            };
            devShell = {
              tools = hp: {
                inherit (hp)
                  ghcid
                  cabal-fmt
                  haskell-language-server
                  ormolu
                  hlint
                  ;
              };
            };
            autoWire = [
              "packages"
              "apps"
              "devShells"
            ];
          };
          packages.default = self'.packages.maybe-game;
          apps.default = self'.apps.maybe-game;
        };
    };
}
