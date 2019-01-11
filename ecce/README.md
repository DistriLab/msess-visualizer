# Installation
Run this command in the same folder as this README file:
```
cabal install
```

# Usage
## General
1. When entering filenames, make sure not to use tab completion. If you do, the
   file will most likely not exist.
```
Welcome!
Type "help" for more information.

ecce> load processor.test
> s
*** Exception: Parse error: ["processor.test : openFile: does not exist
(No such file or directory)"]
λ> main
Welcome!
Type "help" for more information.

ecce> load processor.test
> s
...
```

## Parser.hs
### Running
```
ghci Parser.hs
*Parser> main
```

### Example Execution
```
Welcome!
Type "help" for more information.

ecce> help
Here are a list of commands:
help
load
test
ecce> test formula
#0:     P
#1:     P
ecce>
```

## Processor.hs
### Running
```
ghci Processor.hs
*Processor> main
```

### Example Execution
```
Welcome!
Type "help" for more information.

ecce> help
Here are a list of commands:
help
load
ecce> load test/processor/example
> s
> s
> s
> s
...
> q
Quitting
ecce>
```

## Main.hs
Within the GUI, use the same keybindings as for `Processor.hs`.

### Performance
Running either `Frontend.hs` or `Main.hs` in `ghci` often takes 100% CPU. To 
reduce CPU (and memory) usage, compile the Main module to generate the 
executable (`ghc Main.hs`), and then run the compiled executable (`Main`).

# Notes
## Repository Structure
The default branch is the base implementation.
Every other branch is a modification of the default branch.
Every branch has a `patch-notes.md` that has:
- Title: the name of the branch
- Description: what the branch modifies
The idea is:
- The default branch is the simplest implementation with less code and less 
  features. Code is easier to read and understand.
- If users want a specific feature, they checkout a branch, generate a diff 
  patch against the default branch, and apply the changes in that patch.
