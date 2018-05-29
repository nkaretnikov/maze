(import ./reflex-platform {}).project ({ pkgs, ... }: {
  packages = {
    maze = ./.;
  };

  shells = {
    ghcjs = ["maze"];
  };
})
