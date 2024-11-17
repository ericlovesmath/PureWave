{
  description = "PureWave: Haskell Synthesizer";
  inputs = {
    nixpkgs-unstable.url = "github:NixOS/nixpkgs/nixos-unstable";
  };
  outputs =
    { nixpkgs-unstable, ... }:
    let
      system = "aarch64-darwin";
    in
    {
      devShells.${system}.default =
        let
          pkgs = import nixpkgs-unstable { inherit system; };
        in
        pkgs.mkShell {
          packages = with pkgs; [
            (haskellPackages.ghcWithPackages (hp: with hp; [
              random
            ]))
          ];
        };
    };
}
