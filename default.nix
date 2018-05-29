(import ./reflex-platform {}).project ({ pkgs, ... }: {
  packages = {
    maze = ./.;
    # nix-prefetch-url --unpack https://github.com/OWNER/REPO/archive/REV.tar.gz
    reflex-dom-canvas = pkgs.fetchFromGitHub {
      owner = "qfpl";
      repo = "reflex-dom-canvas";
      rev = "fcabeaf63c60753e0dbb07aab93bc1d2b11db9b8";
      sha256 = "05b0s1k1a144jyl0zg8crpiqwfl710ncdzyswm4cszrsdfz00rc0";
    };
  };

  shells = {
    ghcjs = ["maze"];
  };
})
