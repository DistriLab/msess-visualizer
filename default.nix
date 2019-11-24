{ system ? builtins.currentSystem }:
(import /home/koo/github/reflex-platform { inherit system; }).project ({ pkgs, ...  }: {
  packages = {
    ecce = ./ecce;
    ecce-frontend = ./ecce-frontend;
    ecce-haskeline = ./ecce-haskeline;
  };

  shells = {
    ghc = ["ecce" "ecce-haskeline"];
    ghcjs = ["ecce-frontend"];
  };
})
