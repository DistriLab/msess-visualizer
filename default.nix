{ system ? builtins.currentSystem }:
(import /home/koo/github/reflex-platform { inherit system; }).project ({ pkgs, ...  }: {
  packages = {
    ecce = ./ecce;
    ecce-frontend = ./ecce-frontend;
  };

  shells = {
    ghc = ["ecce"];
    ghcjs = ["ecce-frontend"];
  };
})
