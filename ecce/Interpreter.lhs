\defslide{InterpreterIntroduction}{
Abstracted textual interface between |ecce| and the user.
\begin{itemize}
  \item Configures a mapping between commands and functions.
  \item Mapping matches first word from user, mapped function executes on the
  rest of the user input.
\end{itemize}
}

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

\defslide{InterpreterOutput}{
The |Output| function prints a tuple of information.

\begin{code}
type Output = (String, [String], String) -> IO ()
\end{code}
}

%if False
\begin{code}
extractParse :: SParsec a -> String -> Either ParseError a
extractParse p s = Text.Parsec.parse p "" s
\end{code}
%endif

%if False
We use |Output| to define two interpreters with two input-output (IO)
backends: either the |Haskeline| library, or Haskell's usual monadic IO.
|Haskeline| includes many more textual interface features, like allowing
us to delete our previous inputs.  |Haskeline| is also used in
|ghci|, therefore we recommend |Haskeline| as an interpreter, to
provide a uniform text-input experience to |ghci| users.
\par
|mainRegular| performs an |IO ()|.
\par
|mainHaskeline| wraps an |InputT IO ()| object with
|defaultSettings| in a |runInputT|.

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

The non-|Haskeline| interpreter is defined in the conventional manner:
firstly, print a prompt, then get user input, |interpret| user input,
and lastly repeat.

\begin{code}
interpreterRegular :: [(String, Output)] -> Output -> IO ()
interpreterRegular commandOutputs incommandOutput = do
  putStr "ecce> "
  inputLine <- getLine
  interpret commandOutputs incommandOutput inputLine
  interpreterRegular commandOutputs incommandOutput
\end{code}

The |Haskeline| interpreter works similarly, but uses different
functions.  Prompt printing and getting user input is performed in a single
call to |getInputLine|.  While the non-|Haskeline| interpreter
gets user input as a |String|, the |Haskeline| interpreter
returns a |Maybe| object: either the user gave some input, or not.  We
thus define two behaviors on |Maybe|: if it is |Nothing|, then we
print a departing message.  If it contains a user input, then we firstly
interpret that input, and then repeat the |Haskeline| interpreter.  We
take the return value of the interpretation, and use |liftIO| to change
the |IO| context to the environmental monadic context.  The
environmental monadic context is determined by the type of the other
monadically composed object.  In this case, the monadic composition is
performed by |>>|, and the monadically composed object is the
|Haskeline| interpreter itself, which we know has a |InputT IO|
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
%endif

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

\defslide{InterpreterInterpretDesc}{
|interpret| parses the first word of |inputLine| as a command, then looks up
this command in the |commandOutputs| mapping to get the corresponding |Output|
function.
\begin{itemize}
  \item For each |String| in |commands|, we create a parser that parses only
  that |String|.  We try each parser on |inputLine|, and the parsed value is
  stored in |command|.
  \item The order of trying parsers matters.
  \item If |command| does not exist in the |commandOutputs| mapping, then we
  pass the rest of the input line to be processed by |incommandOutput|.
  Otherwise, if |command| exists in the mapping, then we use the mapped
  |Output| to process the rest of the input line.
\end{itemize}
The selected |Output| function defines the behavior of
|interpret|.
}

\defslide{InterpreterInterpretCode}{
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
\end{code}
}

%if False
parseCommand :: [String] -> SParsec String
parseCommand commands = foldl (\p p' -> p <|> try p') (try h) t
  where
    (h:t) = map string commands

parseRestInputLine :: [String] -> SParsec String
parseRestInputLine commands =
  parseCommand commands >> many space >> many anyChar
%endif

\section{Interpreter}
\slide{InterpreterIntroduction}
\slide{InterpreterOutput}
\slide{InterpreterInterpretDesc}
\slide{InterpreterInterpretCode}
