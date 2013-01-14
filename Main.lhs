%%
%% Main.lhs
%% Made by nicuveo <crucuny@gmail.com>
%%



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Header

\documentclass[mathserif,dvipsnames,9pt]{beamer}

\usepackage{beamerthemesplit}
\usepackage[american]{babel}
\usepackage[latin1]{inputenc}
\usepackage[T1]{fontenc}
\usepackage{listings}
\usepackage{graphicx}
\usepackage{colortbl}
\usepackage{listings}
\usepackage{floatflt}
\usepackage{wasysym}
\usepackage{wrapfig}
\usepackage{txfonts}
\usepackage{hhline}
\usepackage{layout}
\usepackage{times}
\usepackage{bbm}

\title{Game of Life: an introduction to Haskell}
\author{Antoine Leblanc \\ \small{antoine.jp.leblanc@gmail.com}}
\institute{\footnotesize{\emph{Haskell Paris}}}
\date{January, 2013}



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Customisation

\input{theme}

\newcommand{\Haskell}{\textsc{Haskell}}
\newcommand{\Alive}{\structure{alive}}
\newcommand{\Dead}{\structure{dead}}



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Document

\begin{document}



%% Title frame

\begin{frame}[fragile]
\titlepage
\end{frame}


%% Intro

\section*{Introduction}

\subsection*{Foreword}

\begin{frame}
  \frametitle{Disclaimer}
  \begin{center}
    \textsc{\textbf{Warning}}
  \end{center}
  ~\\
  This presentation is the result of a beginner's first steps. Example code
  should \alert{NOT} be held as reference.
  ~\\ ~\\
  \begin{center}
    \alt<2>{\includegraphics[height=80pt]{img/no_idea}}
           {\vspace{70pt}}
  \end{center}

\end{frame}

\begin{frame}[fragile]
  \frametitle{Literate programming}
  \Haskell{} and literate programming:
  ~\\
  \begin{itemize}
    \pause
  \item Built-in: \verb+.lhs+ file.
  \item \LaTeX{} mode (\verb+\begin{code}+) or bird mode (\verb+> code+).
    \pause
  \item \verb+runhaskell hello_world.lhs+ $\rightarrow$ \verb+``Hello world!''+
  \item \verb+pdflatex hello_world.lhs+ $\rightarrow$ \verb+hello_world.pdf+
    \pause
  \item Those slides are also a valid \Haskell{} program.
  \end{itemize}
\end{frame}



%% Main

\section{Objective}

\subsection{A Game of Life}

\begin{frame}
  \frametitle{Life as a cellular automaton}
  A \emph{Game of Life}\ldots
  ~\\
  \begin{itemize}
  \item Determinist ``game'' based on a $n * m$ matrix.
  \item Each cell is either \Alive{} or \Dead{}.
  \item From a given state $t$, a state $t + 1$ is computed using the following rules:\\
    \pause
    \begin{itemize}
      \item an \Alive{} cell surrounded by less than 2 \Alive{} cells dies;
      \item an \Alive{} cell surrounded by more than 3 \Alive{} cells dies;
      \item other \Alive{} cells remain \Alive{}.
    \end{itemize}
    \pause
    \begin{itemize}
      \item a \Dead{} cell surrounded by exactly 3 \Alive{} cells comes to life;
      \item other \Dead{} cells remain \Dead{}.
    \end{itemize}
  \end{itemize}
\end{frame}

\pgfdeclaremask{wikimask}{img/wiki_mask}
\setfooterlogo{img/wiki}{wikimask}
\begin{frame}
  \frametitle{Game examples}
  \begin{columns}[c]
    \begin{column}{5cm}
      \begin{center}
        Blinker pattern; \alt<2>{$t = 1$}{$t = 0$} \\~\\
        \alt<1>{\includegraphics[height=100pt]{img/blinker-0}}
               {\includegraphics[height=100pt]{img/blinker-1}}
      \end{center}
    \end{column}
    \begin{column}{5cm}
      \begin{center}
        Toad pattern; \alt<2>{$t = 1$}{$t = 0$} \\~\\
        \alt<1>{\includegraphics[height=100pt]{img/toad-0}}
               {\includegraphics[height=100pt]{img/toad-1}}
      \end{center}
    \end{column}
  \end{columns}
\end{frame}
\defaultlogo

\subsection{Specifications}

\begin{frame}
  \frametitle{$--$help}
  How should our game behave?
  ~\\~\\
  \begin{itemize}
  \item Input
    \begin{itemize}
      \item<2-> Game description file.
      \item<2-> Control argument.
    \end{itemize}
    ~
  \item Output
    \begin{itemize}
    \item<3-> Rendering of the resulting steps.
    \end{itemize}
    ~
  \item Control
    \begin{itemize}
    \item<4-> Numbers of steps.
    \item<4-> Or unlimited steps, stops on convergence.
    \end{itemize}
  \end{itemize}
\end{frame}

\subsection{Code architecture}

\begin{frame}[fragile]
  \frametitle{Dividing into modules}
  Our code might be divided in four parts:
  \begin{itemize}
  \item A matrix module.
  \item A game module containing the rules.
  \item A parser that creates a game instance from a given string.
  \item A \verb+main+ control loop that binds all this together.
  \end{itemize}
\end{frame}



\section{Matrix module}
\input{Matrix.lhs}



\section{Game rules}
\input{Game.lhs}



\section{Board parser}
\input{Parser.lhs}



\section{Putting it all together}

\subsection{Objective}

\begin{frame}[fragile]
\frametitle{Now what?}

\begin{itemize}
\item We know have \structure{pure} modules to manipulate our game.
\item Time to chain it with IO!
\end{itemize}

\end{frame}

\subsection{Code review}

\begin{frame}[fragile]
\frametitle{Headers}
\begin{block}{}
\begin{code}
import Control.Monad(when)
import Text.Parsec.String(parseFromFile)
import System.Environment
import System.Exit
import Data.Either
import Data.List
import Matrix
import Parser
import Game
\end{code}
\end{block}
~\\
A whole lot of \verb+import+s.
~\\
\begin{itemize}
\item Selective import \verb+Module(importedFunction)+.
\item Qualified import \verb+qualified Module+.
\item Alias import \verb+Module as M+.
\end{itemize}
~\\
\end{frame}



\begin{frame}[fragile]
\frametitle{Non-trivial function}
\begin{block}{}
\begin{code}
filterBoards :: [Board] -> [Board]
filterBoards boards = map fst . takeWhile isValid $ zip boards uniq
  where isValid = snd
        uniq = zipWith notElem boards previous
        previous = scanl (flip (:)) [] boards
\end{code}
\end{block}
~\\
\begin{itemize}
\item \verb+fst ::+ $(a, b) \rightarrow a$
\item \verb+snd ::+ $(a, b) \rightarrow b$
\item \verb+zip+ = \verb+zipWith (,) ::+ $[a] \rightarrow [b] \rightarrow [(a, b)]$
\end{itemize}
\end{frame}



\begin{frame}[fragile]
\frametitle{Boards generation function}
\begin{block}{}
\begin{code}
result :: Int -> Board -> [Board]
result steps board
  | steps  > 0  = take (steps + 1) fullGame
  | steps == 0  = limitedGame
  | otherwise   = fullGame
  where fullGame = game board
        limitedGame = filterBoards fullGame
\end{code}
\end{block}
~\\
\begin{itemize}
\item steps > 0 $\Rightarrow$ $steps$ first boards
\item steps == 0 $\Rightarrow$ all boards until cycle
\item otherwise $\Rightarrow$ unlimited boards
\end{itemize}
\end{frame}



\begin{frame}[fragile]
\frametitle{Prettification}
\begin{block}{}
\begin{code}
prettify :: Board -> String
prettify = unlines . rows . fmap toChar
  where toChar Alive = 'o'
        toChar Dead  = ' '

separator :: Int -> String
separator = (flip replicate) '-'
\end{code}
\end{block}
~\\
Moar point free style!
\end{frame}



\begin{frame}[fragile]
\frametitle{IO actions}
\begin{block}{}
\begin{code}
run :: Int -> Board -> IO ()
run steps board = sequence_ output
  where output = intersperse (putStrLn sepLine) prettyResult
        prettyResult = map (putStrLn . prettify) (result steps board)
        sepLine = separator (width board)
\end{code}
\end{block}
~\\
\begin{itemize}
\item \verb+IO ()+: action.
\item \verb+sequence :: [IO ()]+ $\rightarrow$ \verb+IO ()+
\end{itemize}
\end{frame}



\begin{frame}[fragile]
\frametitle{IO actions}
\begin{block}{}
\begin{code}
main = do
  args <- getArgs
  when (length args /= 2) exitFailure
  let steps = read (args !! 1) :: Int
  board <- parseFromFile parseBoard (args !! 0)
  either (error . show) (run steps) board
\end{code}
\end{block}
~\\
And that's all folks!
\end{frame}



\section{Final remarks}
\subsection{Conclusion}
\begin{frame}[fragile]
\frametitle{Conclusion}
~\\
A few final words:
\begin{itemize}
\item This code is quite concise.
\item But it lacks tests!
\item \Haskell{} is fun. \smiley
\pause
\item \alert{Your turn now!}
\end{itemize}
\end{frame}



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% End

\end{document}
