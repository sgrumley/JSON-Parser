{\tt data}


\begin{code}
module Main (main) where
\end{code}

\begin{code}
import System.Environment
\end{code}

\begin{code}
import ABR.Util.Pos
import ABR.Parser
import ABR.Parser.Lexers
\end{code}


This type represents a JSON Object.

\begin{code}
data JValue
    = JNumber Float
    | JString String
    | JArray[JValue]
    | JObject [(String, JValue)]
    | JBool Bool
    deriving(Show,Eq)


\end{code}

\begin{code}
data JSON =  JValue JValue
               deriving Show
\end{code}
