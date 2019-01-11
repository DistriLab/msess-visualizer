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
