\subsection{Interpreter}

The interpreter provides a high-level abstraction of the textual interface 
between \textit{ecce} and the user.  The library user configures a mapping 
between commands and functions.  Then, when the first word from the user 
matches a command in the mapping, the mapped function will be executed on the 
rest of the user input.

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
modules use.  We define an \textit{Output} function that prints a tuple
of information.  We will explain how this function is used later.

\begin{code}
type Output = (String, [String], String) -> IO ()
\end{code}

%if False
\begin{code}
extractParse :: SParsec a -> String -> Either ParseError a
extractParse p s = Text.Parsec.parse p "" s
\end{code}
%endif

We use \textit{Output} to define two interpreters with two input-output (IO)
backends: either the \textit{Haskeline} library, or Haskell's usual monadic IO.
\textit{Haskeline} includes many more textual interface features, like allowing
us to delete our previous inputs.  \textit{Haskeline} is also used in
\textit{ghci}, therefore we recommend \textit{Haskeline} as an interpreter, to
provide a uniform text-input experience to \textit{ghci} users.
\par
\textit{mainRegular} performs an \textit{IO ()}.
\par
\textit{mainHaskeline} wraps an \textit{InputT IO ()} object with
\textit{defaultSettings} in a \textit{runInputT}.

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

The non-\textit{Haskeline} interpreter is defined in the conventional manner:
firstly, print a prompt, then get user input, \textit{interpret} user input,
and lastly repeat.

\begin{code}
interpreterRegular :: [(String, Output)] -> Output -> IO ()
interpreterRegular commandOutputs incommandOutput = do
  putStr "ecce> "
  inputLine <- getLine
  interpret commandOutputs incommandOutput inputLine
  interpreterRegular commandOutputs incommandOutput
\end{code}

The \textit{Haskeline} interpreter works similarly, but uses different
functions.  Prompt printing and getting user input is performed in a single
call to \textit{getInputLine}.  While the non-\textit{Haskeline} interpreter
gets user input as a \textit{String}, the \textit{Haskeline} interpreter
returns a \textit{Maybe} object: either the user gave some input, or not.  We
thus define two behaviors on \textit{Maybe}: if it is \textit{Nothing}, then we
print a departing message.  If it contains a user input, then we firstly
interpret that input, and then repeat the \textit{Haskeline} interpreter.  We
take the return value of the interpretation, and use \textit{liftIO} to change
the \textit{IO} context to the environmental monadic context.  The
environmental monadic context is determined by the type of the other
monadically composed object.  In this case, the monadic composition is
performed by \textit{>>}, and the monadically composed object is the
\textit{Haskeline} interpreter itself, which we know has a \textit{InputT IO}
context.
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

Here, we explain how the \textit{Output} function is used.  \textit{interpret}
takes three inputs: a mapping of \textit{String}s to \textit{Output}s
(\textit{commandOutputs}), a single \textit{Output} (\textit{incommandOutput}),
and a \textit{String} (\textit{inputLine}) to be interpreted.  In general,
\textit{interpret} parses the first word of \textit{inputLine} as a command,
then looks up this command in the \textit{commandOutputs} mapping to get the
corresponding \textit{Output} function.  If the command does not exist, then
\textit{interpret} uses \textit{incommandOutput} as a default \textit{Output}
function.  The selected \textit{Output} function defines the behavior of
\textit{interpret}.
\par
We first extract the keys of \textit{commandOutputs} as \textit{commands}, with
type \textit{[String]}.  Then, \textit{parseCommand} behaves like so: for each
\textit{String} in \textit{commands}, we create a parser that parses only that
\textit{String}, using the \textit{string} parser constructor.  We try each
parser on \textit{inputLine}, and the parsed value is stored in
\textit{command}.  There is a shortcoming of this strategy: the order of trying
parsers matters.  If short commands and long commands start with the same
letters, and if we always try to construct the parser for the short commands
first, then we will never construct the parser for the long command.  This is
not a limitation in our current usage of the interpreter, since our commands
have very different letters.  However, this could be a problem if more commands
are defined.
\par
\textit{parseRestInputLine} takes the \textit{inputLine}, and parses it like
\textit{parseCommand}, but ignores the result.  \textit{parseRestInputLine}
also ignores spaces between the command and the rest of \textit{inputLine}.
Finally, it creates a parser that returns the other characters of
\textit{inputLine}.
\par
Each of the constructed parsers returned by \textit{parseCommand} and
\textit{parseRestInputLine} parse the \textit{inputLine}, and on parse failure,
returns a \textit{String} \textit{"error"}.  This is not the best way to handle
parsing failure, since there is no way to differentiate between an actual
parsing failure and a command \textit{"error"} or an input line
\textit{"error"}.  However, this is once again not a problem because we do not
use those commands, and an input of \textit{"error"} would lead to some parsing
errors in the later parts of the program.
\par
Finally, we use the parsed \textit{command} to get the mapped \textit{Output}.
If \textit{command} does not exist in the \textit{commandOutputs} mapping, then
we pass the rest of the input line to be processed by \textit{incommandOutput}.
Otherwise, if \textit{command} exists in the mapping, then we use the mapped
\textit{Output} to process the rest of the input line.

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
