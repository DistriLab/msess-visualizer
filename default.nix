{ system ? builtins.currentSystem }:
(import /home/koo/github/reflex-platform { inherit system; }).project ({ pkgs, ...  }: {
  packages = {
    ecce = ./ecce;
    ecce-gloss = ./ecce-gloss;
  };

  shells = {
    ghc = ["ecce" "ecce-gloss"];
    ghcjs = [];
  };
})
