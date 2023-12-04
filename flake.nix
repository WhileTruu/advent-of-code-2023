{
  description = "roc-server flake";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
  };

  outputs = { self, nixpkgs }:
    let
      pkgs = import nixpkgs { system = "aarch64-darwin"; };

      roc = pkgs.stdenv.mkDerivation {
        pname = "roc";
        version = "0.0.1";
        src = pkgs.fetchurl {
          # nix-prefetch-url this URL to find the hash value
          # url = "https://github.com/roc-lang/roc/releases/download/nightly/roc_nightly-macos_apple_silicon-2022-12-15-29db3bb.tar.gz";
          # sha256 = "0pkb0d0bdplk9shhcd1imm3fphvkbw23bx7a7zmc2gsp6845wx1n";
          
          url = "https://github.com/roc-lang/roc/releases/download/nightly/roc_nightly-macos_apple_silicon-latest.tar.gz";
          sha256 = "1jf66kk7lnhzn558ssvzkjgv9sq6vcf2js6r7xzlf8z4hcliy52c";


        };
        phases = [ "installPhase" ];
        installPhase = ''
          mkdir -p $out/bin
          cd $out/bin && tar -zxf $src --strip-components 1
        '';
      };

    in {
      devShell.aarch64-darwin = pkgs.mkShell {
        buildInputs = [
          roc
          pkgs.elmPackages.elm
          pkgs.cargo
        ];
      };
    };
}
 
