\subsection{Specifications}

\begin{frame}[fragile]
\frametitle{Objective}

\begin{itemize}
  \item Now we want to parse a string and output a game board.
  \item We'll use \structure{Parsec}, a standard library.
  \item Although quite complicated, resulting code is easy to read.
\end{itemize}

\end{frame}



\subsection{Code review}

\begin{frame}[fragile]
\frametitle{Parser header}
\begin{block}{}
\begin{code}
module Parser (parseBoard) where

import Text.Parsec.String
import Text.Parsec
import Matrix
import Game

parseCell '.' = Dead
parseCell '#' = Alive
\end{code}
\end{block}
\end{frame}



\begin{frame}[fragile]
\frametitle{Pseudo-grammar}
\begin{block}{}
\begin{code}
comment = string "//" >> anyChar `manyTill` newline -- :: Parser String
void    = spaces >> optional (comment >> void)      -- :: Parser ()
int     = void >> fmap read (many1 digit)           -- :: Parser Int
cell    = void >> fmap parseCell (oneOf ".#")       -- :: Parser Cell
cells n = count n cell                              -- :: Parser [Cell]

parseBoard :: Parser Board
parseBoard = do
  width  <- int
  height <- int
  cells  <- cells (width * height)
  void
  eof
  return $ fromList width height cells
\end{code}
\end{block}

~\\

\begin{itemize}
  \item Type declarations CAN be omitted.
  \item Parsec being a monad, \structure{do notation} can be used to compose a parser.
  \item Read out loud (\verb+>>+ can be read as ``followed by'').
\end{itemize}

\end{frame}
