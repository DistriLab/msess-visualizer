# Installation
We need `ghcjs`. The easiest way is to first install the `nix` package manager.

Run this command in the same folder as this README file:
```
nix-build -o ecce-result -A ghc.ecce
nix-build -o ecce-frontend-result -A ghcjs.ecce-frontend
```
