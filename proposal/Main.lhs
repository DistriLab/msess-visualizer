\documentclass[acmsmall,10pt,review,anonymous]{acmart}\settopmatter{printfolios=true}
%include Library/polycode.fmt

\input{usages}


\setcopyright{none}
\pagenumbering{arabic}
% DOI
\acmDOI{}

% ISBN
\acmISBN{}

% Conference
\acmConference[]{}{}{}
\acmYear{}
\copyrightyear{}

\acmPrice{}

%\settopmatter{printacmref=false} % Removes citation information below abstract
\renewcommand\footnotetextcopyrightpermission[1]{} % removes footnote with conference information in first column
%\pagestyle{plain} % removes running headers

\begin{document}
\captionsetup[figure]{labelfont=bf,textfont=normalfont,singlelinecheck=on}
\setlist[itemize]{leftmargin=*}
\setlist[enumerate]{leftmargin=*}
% \special{papersize=8.5in,11in}
% \setlength{\pdfpageheight}{\paperheight}
% \setlength{\pdfpagewidth}{\paperwidth}


\title{A Visualizer and Simulator for Session Protocols}


\author{}
\affiliation{}
\input{notation.tex}

\begin{abstract}
Distributed software is ubiquitous in today’s digital world. The expert designers of distributed systems often engage formal techniques to provide an unambiguous description of such software. One rigorous trend in describing the communication/synchronization of distributed applications is that of multiparty session protocols. A multiparty session protocol describes the communication interaction between multiple peers, highlighting the order and the kind of transmitted messages.

Coming up with the right specification is often challenging due to the rather complex interaction schemes used within these systems. To ease the task of designing and writing session protocols and to test their feasibility before even implementing them, it would be appropriate to be able to visualize and simulate the intended communication. "One Look Is Worth A Thousand Words" they say! The goal is to have a visualization mechanism of session protocols that goes two ways. On one hand it generates a graphical display of the communication given the formal protocol. On the other hand, it should be able to allow the designer to interact with the graphical protocol such that any modification in the graphic protocol would also be captured by its formal counterpart.

\end{abstract}

\maketitle


\input{style}


\section{Overview}

\begin{center}
\includegraphics[scale=0.6]{overview.png}
\end{center}

In this setting a user should be able to write a global protocol with
any of the following synchronized mechanisms:
(1) formal language in the style of session logic \cite{Andreea2017thesis},
(2) the DSL for visualizing a communication protocol,
(3) the GUI, which should provide a drag-and-drop facility for adding new
communicating parties and or transmissions.

Any change in one of these three layers should be uniformly reflected in
all the layers (either automatically or via a synchronization mechanism).

\subsection{Formal Global Protocols}
\begin{center}
$
\begin{array}{llll}
  \textit{Single ~ transmission}~ & \code{\terminal}& {~ ::=}& \quad \code{\atransmit{S}{R}{v\,{\cdot}\,\fmsg}{\chanvar}{}}\\
  \textit{Global~protocol}& \code{\prot% (\rolevar^*)
  } &{~ ::=} &\code{~~~ \terminal}\\
  \textit{Concurrency} &&& ~|~ \code{\prot\,{\useq}\,\prot} \\
  \textit{Choice} &&& ~|~ \code{\prot\,{\vee}\,\prot } \\
  \textit{Sequencing}
  &&& ~|~ \code{\prot\,{\gseq{}}\,\prot} \\
  % \textit{Guard} &&& ~|~ \code{\guard{\racefreeas}} \\
  % \textit{Assumption} &&& ~|~ \code{\assume{\racefreeas}} \\
  \textit{Inaction} &&&   ~|~ {\emp}
\end{array}
$
\end{center}

\subsection{DSL for Building and Simulating Session Protocols}

\begin{itemize}
  \item should follow that of the Formal Global Protocols as much as
    possible.
  \item split between {\it the static} part for building the visualization
    of the protocol, and {\it the dynamic} part for simulating the
    communication described by the protocol.
\end{itemize}


\noindent
\emph{\bf Static}.
\begin{verbatim}
  {program}       Prog          :== Parties_decl x Channels_decl x Transm
  {parties decl}  Parties_decl  :== Party P*
  {channels decl} Channels_decl :== Channel c*
  {transmissions} Transm        :==  P -(OPT color)-> P: c<msg(v)>
                                   | {Transm} * {Transm}
                                   | Transm ; Transm
                                   | {Transm} \/ {Transm}
                                   | loop(Transm)
                                   | end.
\end{verbatim}

\emph{\bf Dynamic}.

\asay{Should we employ some sort of FRP here? for the simulation part.
  Fro example, Haskell's reactive-banana
  https://wiki.haskell.org/Reactive-banana/Examples
  }

\section{Session Protocols GUI}

\asay{ see examples of interacting with sequenced diagrams
  https://www.websequencediagrams.com }

we further need:
\begin{itemize}
  \item a way to represent arbitrary order / concurrency inter-party
  \item a way to represent arbitrary order / concurrency intra-party
\end{itemize}

\section{Introduction}
\textit{ecce} is a module that deals with structured data and its
transformations.  This can be seen at every level of its implementation.
Parsing fails unless the input string is structured, and the resulting Abstract
Syntax Tree (AST) is structured.  Processing peels off single layers of
structure off of the AST.  Projection is a direct functional mapping between
data types.  Finally, the images shown to the user are also structured.

Functional programming is ideal to deal with such many disparate structures, as
it can express transformations between these structures succinctly.
Furthermore, we can encode bidirectional transformations as partial
isomorphisms, to avoid repeating similar code.

For example, code for parsing and pretty-printing are similar, in the sense
that there is a normal form of the AST that the parser and pretty-printer both
understand.  Encoding this normalized AST in the parser involves correctly
parsing the input string, and encoding this normalized AST in the printer
involves assuming the normalized AST already exists in that exact form.  This
double-encoding is tedious for the programmer to maintain, since a change made
in the parser mandates a corresponding change in the pretty-printer, and
vice-versa.

Partial isomorphisms are constructors that have both the parser and
pretty-printer functions in the same object.  The crucial idea is, the
programmer explicitly defines these two functions only in a base set of partial
isomorphisms, from which more complex partial isomorphisms are formed without
any more explicit definitions.  This work-saving technique is enabled by an
algebra of partial isomorphisms \cite{Rendel}.

\section{Types}
A diagram will summarize the structured data transformations in \textit{ecce}.
So, we give an overview of the type transformations in \textit{ecce} (Fig.
TODO).  Note that partial isomorphisms exist only for the \textit{String}
\leftrightarrow \textit{GlobalProtocol} transformation, since this is the
transformation that uses lexers and parsers already defined in the
\textit{invertible-syntax} package.  Partial isomorphisms for other
transformations are a work in progress.

\begin{center}
\includegraphics[scale=0.4]{../ecce/plantuml/transformations.png}
\end{center}

\section{Modules}
In this section, we give an overview of each module in \textit{ecce}, and
explain the implementation.  The module dependencies are graphed in Fig. TODO.

\begin{center}
\includegraphics[scale=0.5]{../ecce/plantuml/modules.png}
\end{center}

%include ../ecce/Base.lhs
%include ../ecce/Interpreter.lhs
%include ../ecce/Parser.lhs
%include ../ecce/Unparser.lhs
%include ../ecce/Projector.lhs
%include ../ecce/Processor.lhs
%include ../ecce/Frontend.lhs

\section{Conclusion}
Overall, this project was an exercise in transforming structured descriptions.

\subsection{Future Work}
There are a few directions we can expand our work to.
\begin{enumerate}
  \item Continue describing more structure in the frontend, to match the
  structure in the grammar.  For example, to draw a box, whose contents are all
  signified to be concurrent transmissions, even though they may not appear
  concurrent.  This is analogous to what the Unified Modelling Language has
  \cite{TODO}.  We can emphasize the concurrency property of transmissions in
  this box, by reordering these transmissions within the box on a keypress.
  This has the added benefit of allowing the user to visually inspect if their
  concurrent section has undesirable behaviors.  Some other expansions on
  structure need current types to change, e.g. if we want to show the choice of
  protocols to the user, then our processor needs to be able to return both
  \textit{Transmit}s and \textit{GlobalChoice}s.
  \item Make the graphical user interface interactive.  Specifically, clicking
  and dragging to re-order the transmissions.  Dragging can be done by
  registering which graphical object, if any, was pointed to by the mouse and
  offsetting its translation corresponding to the mouse movement.  Bindings
  from the graphical description to the graphical user interface should exist,
  so that the graphical description can be updated to reflect the new
  transmission order.
  \item Introduce communication and event happens-before ordering.  The current
  graphical user interface shows horizontal arrows.  This means that
  transmissions occur sequentially: before a transmission happens, all parties
  are free to receive at all times, and during a transmission, the sender sends
  the transmission without waiting.  However, this does not reflect real world
  comumnications.  We can introduce blocking sends by having downward-slanted
  arrows, meaning that the receive happens after the send.  This allows
  situations where some parties are not available to receive, and communication
  ordering must be introduced to resolve this.  Otherwise, race conditions
  could occur.
  \item Make every transformation have an inverse. This means:
  \begin{enumerate}
    \item Transformations within external packages like
    \textit{reactive-banana} also have to be reversed.
    \item The \textit{Processor} network defined in terms of reactive-banana \textit{Event}s
    and \textit{Behavior}s has to be reversed. We can either define
    the inverse constructors of \textit{Event}s and \textit{Behavior}s (but it
    is not obvious how to), or we can define new deconstructors, that look at
    the stream of \textit{GlobalTransmits} from the GUI and reconstructs a
    stream of \textit{GlobalProtocol}.
  \end{enumerate}
\end{enumerate}

% The bibliography should be embedded for final submission.

%\begin{thebibliography}{}
%\softraggedright
%\end{thebibliography}

%\begingroup
%\softraggedright

\bibliography{ref}
\bibliographystyle{ACM-Reference-Format}
\citestyle{acmauthoryear}

%\bibliographystyle{plain}
%\endgroup

\end{document}
