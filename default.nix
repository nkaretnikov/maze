(import ./reflex-platform {}).project ({ pkgs, ... }: {
  # Disable Haddock since it breaks the build.
  overrides = self: super: {
    reflex-dom-canvas = pkgs.haskell.lib.dontHaddock super.reflex-dom-canvas;
  };

  packages = {
    maze = ./.;
    # nix-prefetch-url --unpack https://github.com/OWNER/REPO/archive/REV.tar.gz
    reflex-dom-canvas = pkgs.fetchFromGitHub {
      owner = "qfpl";
      repo = "reflex-dom-canvas";
      rev = "2e640ae5b759ea8732d098a6ef4bec701dc42bda";
      sha256 = "0a8fpjzpncfsbaqjgw6wgg4saskqc8x71kc1sp1m7f46l5fw11qa";
    };
  };

  shells = {
    ghcjs = ["maze"];
  };
})
