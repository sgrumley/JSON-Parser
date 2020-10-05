The {\tt Main} module always exports the {\tt main} function.

\begin{code}
module Main (main) where
\end{code}

\noindent The {\tt main} function is the entry point.

\begin{code}
main :: IO ()
main = putStrLn "Hello, World!"
\end{code}
