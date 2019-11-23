{ system ? builtins.currentSystem }:
(import /home/koo/github/reflex-platform { inherit system; }).project ({ pkgs, ...  }: {
  packages = {
    ecce = ./ecce;
    ecce-haskeline = ./ecce-haskeline;
    ecce-gloss = ./ecce-gloss;
    ecce-mxgraph = ./ecce-mxgraph;
  };

  shells = {
    ghc = ["ecce" "ecce-gloss" "ecce-haskeline"];
    ghcjs = ["ecce-mxgraph"];
  };
})
