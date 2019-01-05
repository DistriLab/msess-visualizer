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
