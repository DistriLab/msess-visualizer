# Installation
Run this command in the same folder as this README file:
```
cabal install
```

Note: currently produces this error, without any explanation (could be linker 
error):
```
cabal: Error: some packages failed to install:
ecce-0.1.0.0 failed during the building phase. The exception was:
ExitFailure 1
```

# Usage
## General
1. When entering filenames, make sure not to use tab completion. If you do, the
   file will most likely not exist.
```
Welcome!
Type "help" for more information.

ecce> load reactive.test
> s
*** Exception: Parse error: ["reactive.test : openFile: does not exist
(No such file or directory)"]
λ> main
Welcome!
Type "help" for more information.

ecce> load reactive.test
> s
...
```

## Performance
Running `Frontend.hs` in `ghci` often takes 100% CPU. To reduce CPU (and 
memory) usage, compile the source code (`ghc Frontend.hs`), and then run the 
compiled executable.

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

## Reactive.hs
### Running
```
ghci Reactive.hs
*Reactive> main
```

### Example Execution
```
Welcome!
Type "help" for more information.

ecce> help
Here are a list of commands:
help
load
ecce> load test/reactive/example
> s
> s
> s
> s
...
> q
Quitting
ecce>
```

## Frontend.hs
Within the GUI, use the same keybindings as for `Reactive.hs`.
