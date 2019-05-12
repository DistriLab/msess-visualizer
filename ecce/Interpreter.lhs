\subsection{Interpreter}

The interpreter provides a high-level abstraction of the textual interface
between |ecce| and the user.  The library user configures a mapping between
commands and functions.  Then, when the first word from the user matches a
command in the mapping, the mapped function will be executed on the rest of the
user input.

%if False
\begin{code}
{-
 - SECTION PRAGMAS
 -}
{-# LANGUAGE StandaloneDeriving #-} -- Allows `deriving` on its own line
{-# LANGUAGE GADTs #-} -- Allows constrained ASTs
{-# LANGUAGE UndecidableInstances #-} -- Allows >1 StandaloneDeriving
{-# LANGUAGE ScopedTypeVariables #-} -- Allows type signatures in patterns

{-
 - SECTION MODULE
 -}
module Interpreter where

{-
 - SECTION IMPORTS
 -}
import Base (SParsec)
import Control.Monad.IO.Class (liftIO)
import System.Console.Haskeline
  ( InputT
  , defaultSettings
  , getInputLine
  , outputStrLn
  , runInputT
  )
import Text.Parsec (ParseError, (<|>), anyChar, many, space, string, try)
import qualified Text.Parsec (parse)

-- Every output function must have the same inputs
-- So that the interpret function can be generalized
\end{code}
%endif

The interpreter defines a configurable textual interface, that many other
modules use.  We define an |Output| function that prints a tuple of
information.  We will explain how this function is used later.

\begin{code}
type Output = (String, [String], String) -> IO ()
\end{code}

%if False
\begin{code}
extractParse :: SParsec a -> String -> Either ParseError a
extractParse p s = Text.Parsec.parse p "" s
\end{code}
%endif

We use |Output| to define two interpreters with two input-output (IO) backends:
either the |Haskeline| library, or Haskell's usual monadic IO.  |Haskeline|
includes many more textual interface features, like allowing us to delete our
previous inputs.  |Haskeline| is also used in |ghci|, therefore we recommend
|Haskeline| as an interpreter, to provide a uniform text-input experience to
|ghci| users.
\par
|mainRegular| performs an |IO ()|.
\par
|mainHaskeline| wraps an |InputT IO ()| object with |defaultSettings| in a
|runInputT|.

\begin{code}
mainHaskeline :: [(String, Output)] -> Output -> IO ()
mainHaskeline commandOutputs incommandOutput = do
  welcome
  runInputT defaultSettings $
    interpreterHaskeline commandOutputs incommandOutput

mainRegular :: [(String, Output)] -> Output -> IO ()
mainRegular commandOutputs incommandOutput = do
  welcome
  interpreterRegular commandOutputs incommandOutput
\end{code}

Both interpreters print a welcome message.

\begin{code}
welcome :: IO ()
welcome = do
  mapM_ putStrLn $ "Welcome!" : "Type \"help\" for more information." : "" : []
\end{code}

The non-|Haskeline| interpreter is defined in the conventional manner: firstly,
print a prompt, then get user input, |interpret| user input, and lastly repeat.

\begin{code}
interpreterRegular :: [(String, Output)] -> Output -> IO ()
interpreterRegular commandOutputs incommandOutput = do
  putStr "ecce> "
  inputLine <- getLine
  interpret commandOutputs incommandOutput inputLine
  interpreterRegular commandOutputs incommandOutput
\end{code}

The |Haskeline| interpreter works similarly, but uses different functions.
Prompt printing and getting user input is performed in a single call to
|getInputLine|.  While the non-|Haskeline| interpreter gets user input as a
|String|, the |Haskeline| interpreter returns a |Maybe| object: either the user
gave some input, or not.  We thus define two behaviors on |Maybe|: if it is
|Nothing|, then we print a departing message.  If it contains a user input,
then we firstly interpret that input, and then repeat the |Haskeline|
interpreter.  We take the return value of the interpretation, and use |liftIO|
to change the |IO| context to the environmental monadic context.  The
environmental monadic context is determined by the type of the other
monadically composed object.  In this case, the monadic composition is
performed by |>>|, and the monadically composed object is the |Haskeline|
interpreter itself, which we know has a |InputT IO| context.
\par
\begin{code}
interpreterHaskeline :: [(String, Output)] -> Output -> InputT IO ()
interpreterHaskeline commandOutputs incommandOutput = do
  mInputLine <- getInputLine "ecce> "
  maybe
    (outputStrLn "Quitting")
    (\inputLine ->
       (liftIO $ interpret commandOutputs incommandOutput inputLine) >>
       interpreterHaskeline commandOutputs incommandOutput)
    mInputLine
\end{code}

%if False
\begin{code}
-- Parses inputLine for command, parsers extracted from keys of input (1)
-- Looks up parsed command in input (1), to determine correct function to call
-- Inputs:
-- (1) map of commands to output functions
-- (2) output function when map lookup fails
-- (3) inputLine to be interpreted
-- Output:
-- (1) IO action
\end{code}
%endif

Here, we explain how the |Output| function is used.  |interpret| takes three
inputs: a mapping of |String|s to |Output|s (|commandOutputs|), a single
|Output| (|incommandOutput|), and a |String| (|inputLine|) to be interpreted.
In general, |interpret| parses the first word of |inputLine| as a command, then
looks up this command in the |commandOutputs| mapping to get the corresponding
|Output| function.  If the command does not exist, then |interpret| uses
|incommandOutput| as a default |Output| function.  The selected |Output|
function defines the behavior of |interpret|.
\par
We first extract the keys of |commandOutputs| as |commands|, with type
|[String]|.  Then, |parseCommand| behaves like so: for each |String| in
|commands|, we create a parser that parses only that |String|, using the
|string| parser constructor.  We try each parser on |inputLine|, and the parsed
value is stored in |command|.  There is a shortcoming of this strategy: the
order of trying parsers matters.  If short commands and long commands start
with the same letters, and if we always try to construct the parser for the
short commands first, then we will never construct the parser for the long
command.  This is not a limitation in our current usage of the interpreter,
since our commands have very different letters.  However, this could be a
problem if more commands are defined.
\par
|parseRestInputLine| takes the |inputLine|, and parses it like |parseCommand|,
but ignores the result.  |parseRestInputLine| also ignores spaces between the
command and the rest of |inputLine|.  Finally, it creates a parser that returns
the other characters of |inputLine|.
\par
Each of the constructed parsers returned by |parseCommand| and
|parseRestInputLine| parse the |inputLine|, and on parse failure, returns a
|String| |"error"|.  This is not the best way to handle parsing failure, since
there is no way to differentiate between an actual parsing failure and a
command |"error"| or an input line |"error"|.  However, this is once again not
a problem because we do not use those commands, and an input of |"error"| would
lead to some parsing errors in the later parts of the program.
\par
Finally, we use the parsed |command| to get the mapped |Output|.  If |command|
does not exist in the |commandOutputs| mapping, then we pass the rest of the
input line to be processed by |incommandOutput|.  Otherwise, if |command|
exists in the mapping, then we use the mapped |Output| to process the rest of
the input line.

\begin{code}
interpret :: [(String, Output)] -> Output -> String -> IO ()
interpret commandOutputs incommandOutput inputLine =
  maybe
    (incommandOutput (inputLine, commands, restInputLine))
    ($ (inputLine, commands, restInputLine))
    (lookup command commandOutputs)
\end{code}
%if False
\begin{code}
    -- TODO fix double parsing
    -- TODO if "error" is a legitimate command, then if extractParse fails, the
    -- interpreter will not detect the failure
\end{code}
%endif
\begin{code}
  where
    commands = (fst . unzip) commandOutputs
    command =
      either (const "error") id $ extractParse (parseCommand commands) inputLine
    restInputLine =
      either (const "error") id $
      extractParse (parseRestInputLine commands) inputLine

parseCommand :: [String] -> SParsec String
parseCommand commands = foldl (\p p' -> p <|> try p') (try h) t
  where
    (h:t) = map string commands

parseRestInputLine :: [String] -> SParsec String
parseRestInputLine commands =
  parseCommand commands >> many space >> many anyChar
\end{code}
