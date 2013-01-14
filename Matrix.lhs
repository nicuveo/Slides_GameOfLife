\subsection{Specifications}

\begin{frame}[fragile]
\frametitle{Objective}

We want to write a \verb+Matrix+ module. \\
\begin{itemize}
  \item It should export a \verb+Matrix+ type and some basic
    functions operating on it, including lookup by index and alteration.
  \item It should be a generic type and should therefore implement some
    common container functions.
\end{itemize}


\end{frame}

\subsection{Code review}

\begin{frame}[fragile]
\frametitle{Module definition}
\begin{block}{}
\begin{code}
module Matrix (Matrix(width, height, values),
               Index, Indexed,
               new, fromList,
               xIndices, yIndices, indices, enumerate,
               isIn, neighbours,
               column, row, columns, rows,
               (!), (//), (///)) where

import Control.Applicative
\end{code}
\end{block}

~\\

This is a module header (and an import statement).
\begin{itemize}
\item It declares the module name (\alert{filename must match!}).
\item It declares what the module exports.
\end{itemize}

\end{frame}


\begin{frame}[fragile]
\frametitle{Types}
\begin{block}{}
\begin{code}
data Matrix a = Matrix { width :: Int, height :: Int, values :: [a] } deriving (Show, Eq)
type Index = (Int, Int)
type Indexed a = (Index, a)
\end{code}
\end{block}

~\\

Those are type declarations.

\begin{itemize}
\item \verb+Matrix+ type is made of a width, a height and a list of arbitrary 'a' values. Its constructor is also named \verb+Matrix+.
\item \verb+Index+ type is equivalent to a pair of integers.
\item \verb+Indexed+ type is equivalent to a pair of an \verb+Index+ and an arbitrary 'a' value.
\end{itemize}

\verb+Matrix+ record notation automagically generates the corresponding named accessors.

\end{frame}


\begin{frame}[fragile]
\frametitle{Classes}
\begin{block}{}
\begin{code}
instance Functor Matrix where
  fmap f (Matrix w h m) = Matrix w h (map f m)
\end{code}
\end{block}

~\\

This is an \structure{instance} declaration.\\

~\\

The \verb+Matrix+ type is made an instance of the predefined \structure{Functor}
typeclass. It implements \verb+fmap+, which enables one to call a function over the values of a matrix.

~\\

\verb+fmap+ is the generalization of the \verb+map+ function over lists.

~\\

Additionally, our \verb+Matrix+ type has been made an instance of \verb+Show+ and \verb+Eq+ via the \verb+deriving+ instruction.

\end{frame}



\begin{frame}[fragile]
\frametitle{Constructors}
\begin{block}{}
\begin{code}
new :: Int -> Int -> a -> Matrix a
new w h a = Matrix w h (replicate (w * h) a)

fromList :: Int -> Int -> [a] -> Matrix a
fromList w h a = Matrix w h (take (w * h) a)
\end{code}
\end{block}

~\\

Those are (exported) constructors.\\

~\\

As the internal constructor of the \verb+Matrix+ type isn't exported,
that makes it an \structure{abstract} type. This enforces the use of the
only available exported constructors, therefore successfully hiding the
implementation. (Think \verb+public+ vs \verb+private+.)
\end{frame}



\begin{frame}[fragile]
\frametitle{Accessors (1 / 4)}
\begin{block}{}
\begin{code}
xIndices :: Matrix a -> [Int]
xIndices m = [0..w-1] where w = width m

yIndices :: Matrix a -> [Int]
yIndices m = [0..h-1] where h = height m

indices :: Matrix a -> [Index]
indices m = (flip (,)) <$> (yIndices m) <*> (xIndices m)

enumerate :: Matrix a -> [Indexed a]
enumerate m = map makeIndexed (indices m)
  where makeIndexed index = (index, m ! index)
\end{code}
\end{block}

~\\

\begin{itemize}
\item \verb+flip+ reverses the order of the arguments of a binary function.
\item \verb+(,)+ is the pair constructor of type $a \rightarrow b \rightarrow (a, b)$.
\item \verb+<$>+ is an infix \verb+fmap+ and \verb+<*>+ a ``boosted'' \verb+<$>+.
\item The \verb+where+ notation can also introduce functions.
\end{itemize}
\end{frame}



\begin{frame}[fragile]
\frametitle{Accessors (2 / 4)}
\begin{block}{}
\begin{code}
isIn :: Index -> Matrix a -> Bool
isIn (x, y) m = x `elem` (xIndices m) && y `elem` (yIndices m)

neighbours :: Matrix a -> Index -> [a]
neighbours m p@(x, y) = [m ! np
                        | dx <- [-1..1], dy <- [-1..1],
                          let np = (x + dx, y + dy),
                          np /= p, np `isIn` m]
\end{code}
\end{block}

~\\

\begin{itemize}
\item A binary function can be used as \structure{infix} if surrounded by backquotes.
\item It is possible to do \structure{pattern matching} in function arguments.
\item The \structure{@ notation} allows to name a whole pattern.
\item List comprehension can be quite ugly. \smiley
\end{itemize}
\end{frame}


\begin{frame}[fragile]
\frametitle{Accessors (3 / 4)}
\begin{block}{}
\begin{code}
column :: Matrix a -> Int -> [a]
column m c
  | validInput = [v | ((x, _), v) <- enumerate m, x == c]
  | otherwise  = error $ "Matrix.column: out of bounds column " ++ (show c)
  where validInput = c `elem` (xIndices m)

row :: Matrix a -> Int -> [a]
row m r
  | validInput = [v | ((_, y), v) <- enumerate m, y == r]
  | otherwise  = error $ "Matrix.row: out of bounds row " ++ (show r)
  where validInput = r `elem` (yIndices m)
\end{code}
\end{block}

~\\

\begin{itemize}
\item \verb+_+ means ``whatever'' or ``anything''.
\item \verb+|+ are \structure{guards}, \verb+otherwise+ means \structure{True}.
\item \verb+error+ is somewhat equivalent to \verb+assert+.
\item Those functions, albeit pure, are \structure{partial} functions.
\end{itemize}
\end{frame}


\begin{frame}[fragile]
\frametitle{Accessors (4 / 4)}
\begin{block}{}
\begin{code}
columns :: Matrix a -> [[a]]
columns = fmap <$> column <*> xIndices

rows :: Matrix a -> [[a]]
rows m = row m <$> yIndices m
\end{code}
\end{block}

~\\

\begin{itemize}
\item Those two functions work the same way.
\item It introduces \structure{point free} style.
\item \verb+row m+ is an example of \structure{partial application}.
\pause
\item \alert{Avoid over-tricky code!}
\end{itemize}
\end{frame}


\begin{frame}[fragile]
\frametitle{Operators (1 / 2)}
\begin{block}{}
\begin{code}
(!) :: Matrix a -> Index -> a
(!) m p@(x, y)
  | p `isIn` m = (values m) !! i
  | otherwise  = error $ "Matrix.!: out of bounds indices " ++ (show p)
  where i = (y * w + x)
        w = width m

(//) :: Matrix a -> Indexed a -> Matrix a
(//) m (p@(x, y), n)
  | p `isIn` m = fromList w h ((take i v) ++ [n] ++ (drop (i + 1) v))
  | otherwise  = error $ "Matrix.//: out of bounds indices " ++ (show p)
  where i = (y * w + x)
        v = values m
        h = height m
        w = width m
\end{code}
\end{block}

\begin{itemize}
\item \verb+!+ operator's name is based on lists \verb+!!+ operator.
\item Modifier \verb+//+ returns a newly created matrix.
\end{itemize}
\end{frame}



\begin{frame}[fragile]
\frametitle{Operators (2 / 2)}
\begin{block}{}
\begin{code}
(///) :: Matrix a -> [Indexed a] -> Matrix a
(///) m = foldl (//) m

-- (///) m []     = m
-- (///) m (c:cs) = (m // c) /// cs
\end{code}
\end{block}

~\\

\begin{itemize}
\item \verb+--+ introduces a comment.
\item Introducing \verb+foldl+.
\end{itemize}
\end{frame}

\subsection{Lessons learned}
\begin{frame}[fragile]
\frametitle{Summing it up}

\begin{itemize}
\item Capitalization matters!
\item Hard equilibrium between cleverness and verbosity.
\end{itemize}
\pause
\begin{itemize}
\item This small module implements everything we need.
\item It is \structure{pure}.
\item It is \alert{highly inefficient}!
\end{itemize}
\pause
\begin{itemize}
\item $\Rightarrow $ \verb+Ix.Array+?
\item $\Rightarrow $ \verb+Data.DList+?
\item Use \structure{Hoogle} and \structure{Hayoo}!
\end{itemize}

\end{frame}
