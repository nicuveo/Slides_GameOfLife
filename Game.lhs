\subsection{Specifications}

\begin{frame}[fragile]
\frametitle{Objective}

We now want to write the rules of our game. For that we'll need:
\begin{itemize}
  \item A \verb+Cell+ type, keeping track of a cell's state.
  \item A \verb+Board+ type.
  \item A transition function.
\end{itemize}

\end{frame}



\subsection{Code review}

\begin{frame}[fragile]
\frametitle{Game module and types}
\begin{block}{}
\begin{code}
module Game (Cell(..), Board,
             toggle, alive, dead,
             next, game) where

import Control.Applicative
import Matrix

data Cell = Alive | Dead deriving (Show, Eq, Enum)
type Board = Matrix Cell
\end{code}
\end{block}

~\\

\begin{itemize}
\item \verb+Cell+ is a basic algebraic type.
\item A \verb+Cell+ variable can only hold two values, \structure{Alive} or \structure{Dead}.
\item \verb+Board+ is defined as a cell matrix.
\end{itemize}
\end{frame}



\begin{frame}[fragile]
\frametitle{Cell classes}
\begin{block}{}
\begin{code}
toggle :: Cell -> Cell
toggle Alive = Dead
toggle Dead = Alive

alive :: Cell -> Bool
alive = (==) Alive

dead :: Cell -> Bool
dead = not . alive
\end{code}
\end{block}

~\\

No comment. \smiley

\uncover<2->{~\\ Although \structure{partial application} and \structure{dot notation} are worth noting.}
\end{frame}



\begin{frame}[fragile]
\frametitle{Helper function}
\begin{block}{}
\begin{code}
nextCell :: Board -> Indexed Cell -> Cell
nextCell board (pos, cell)
  | alive cell && alives `notElem` [2, 3] = Dead
  | dead cell && alives == 3              = Alive
  | otherwise                             = cell
  where alives  = count alive neighbs
        count f = length . filter f
        neighbs = neighbours board pos
\end{code}
\end{block}


~\\

\begin{itemize}
\item \verb+nextCell+ computes the next step of a cell.
\item A bit verbose but easy to read.
\end{itemize}
\end{frame}



\begin{frame}[fragile]
\frametitle{Transition functions}
\begin{block}{}
\begin{code}
next :: Board -> Board
next board = fromList w h (nextCell board <$> enumerate board)
  where w = width board
        h = height board

game :: Board -> [Board]
game = iterate next
\end{code}
\end{block}

~\\

\begin{itemize}
\item \verb+next+ maps \verb+nextCell+ over a matrix to compute the next step.
\item \verb+game+ \structure{lazily} generates an infinite list of successive boards.
\pause
\item \alert{Design issue}: we rely on the order of enumerate.
\end{itemize}
\end{frame}
