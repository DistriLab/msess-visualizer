\documentclass{beamer}
%include Library/polycode.fmt
%include Library/beamer.fmt

\usepackage[geometry]{ifsym}
\newcommand{\clap}[1]{\hbox to0pt{\hss#1\hss}}
\renewcommand{\diamond}[2][0.25ex]{\hbox to 1.5em{\hfil\clap{\raisebox{-0.45ex}{\BigDiamondshape}}\clap{\raisebox{#1}{\scriptsize #2}}\hfil}}
\newcommand{\rdiamond}[2][0.25ex]{\hbox to 1.5em{\hfil\clap{\raisebox{-0.45ex}{\ \TriangleRight}}\clap{\raisebox{#1}{\scriptsize #2}}\hfil}}
\newcommand{\ldiamond}[2][0.25ex]{\hbox to 1.5em{\hfil\clap{\raisebox{-0.45ex}{\TriangleLeft\ }}\clap{\raisebox{#1}{\scriptsize #2}}\hfil}}

%format delta = "\delta{}"
%format >=> = ">\!\!\!\!\:\!=\!\!\!\!\!\:\!>"
%format <+> = "\diamond{$+$}"
%format <-> = "\diamond{$-$}"
%format <|> = "\diamond{$|$}"
%format <$> = "\diamond[0ex]{\rm\$}"
%format <*> = "\diamond{$\ast$}"
%format *> = "\rdiamond{$\ast$}"
%format <* = "\ldiamond{$\ast$}"


\newcommand{\defslide}[2]{
\frame<hidden>[label=#1]{#2}
}

\newcommand{\slide}[1]{\againframe<1->{#1}}

\usetheme{Singapore}

\title{ecce}
\subtitle{A Visualizer and Simulator for Session Protocols}
\author{Koo Zhengqun}
\date{\today}

\begin{document}

\defslide{Overview}{
\begin{center}
\includegraphics[scale=0.6]{overview.png}
\end{center}

Write a global protocol with these mechanisms:
\begin{enumerate}
  \item formal session logic,
  \item a processor that executes each step of the protocol,
  \item the GUI.
\end{enumerate}

A change should be reflected in other layers by a synchronization mechanism.
}

\defslide{ecce}{
Transforms structured data.
\begin{itemize}
  \item Input string
  \item AST
  \item Processing
  \item Image data
\end{itemize}
}

\defslide{Types}{
\begin{center}
\includegraphics[scale=0.175]{../ecce/plantuml/transformations.png}
\end{center}
}

\defslide{Modules}{
\begin{center}
\includegraphics[scale=0.275]{../ecce/plantuml/modules.png}
\end{center}
}

\defslide{Future Work}{
There are a few directions we can expand our work to.
\begin{enumerate}
  \item Continue describing more structure in the frontend, to match the
  structure in the grammar.
  \item Make the graphical user interface interactive.
  \item Introduce communication and event happens-before ordering.
  \item Make every transformation have an inverse. This means:
  \begin{enumerate}
    \item Transformations within external packages like
    \textit{reactive-banana} also have to be reversed.
    \item The \textit{Processor} network defined in terms of reactive-banana
    \textit{Event}s and \textit{Behavior}s has to be reversed.
  \end{enumerate}
\end{enumerate}
}

\frame{\titlepage}
\section{Overview}
\slide{Overview}
\slide{ecce}
\slide{Types}
\slide{Modules}

%include ../ecce/Base.lhs
%include ../ecce/Interpreter.lhs
%include ../ecce/Parser.lhs
%include ../ecce/Unparser.lhs
%include ../ecce/Projector.lhs
%include ../ecce/Processor.lhs
%include ../ecce/Frontend.lhs

\section{Future Work}
\slide{Future Work}

\begin{frame}[allowframebreaks]
  \frametitle<presentation>{Further Reading}
  \begin{thebibliography}{10}
  \beamertemplatebookbibitems
  \bibitem{Andreea2017thesis}
    C.~Andreea.
    \newblock {\em A Session Logic for Relaxed Communication Protocols}.
    \newblock National University of Singapore, 2017.
  \beamertemplatearticlebibitems
  \bibitem{Rendel}
    T.~Rendel, K. Ostermann.
    \newblock {\em Invertible Syntax Descriptions: Unifying Parsing and Pretty
    Printing}
    \newblock University of Marburg, 2010.
  \end{thebibliography}
\end{frame}

\end{document}
