{\tt json} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

The program {\tt json} is an algebraic calculator.

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


\EBNFInput{syntax/identifier.ebnf}
Text about the lexer
\begin{code}
arrayL :: Lexer
arrayL = literalL '[' %> "StartOfArray" 
    <|> literalL ']' %> "EndOfArray" 
\end{code}

\begin{code}
objectL :: Lexer
objectL = literalL '{' %> "StartOfObject"
    <|> literalL '}' %> "EndOfObject"
    <|> literalL ':' %> "PairDelimiter" 
\end{code}

\begin{code}
repeatableL :: Lexer
repeatableL = literalL ',' %> "AnotherValue"
\end{code}

\begin{code}
programL :: Lexer
programL = dropWhite $ nofail $ total $ listL 
    [whitespaceL, floatL, repeatableL, stringL, arrayL, objectL]
\end{code}


Text about the Parser
\begin{code}
jstringP :: Parser String
jstringP = tagP "string"
    @> (\(_,s,_) -> s)
\end{code}

\begin{code}
keyValP :: Parser (String, JValue)
keyValP = jstringP            --tagP "string"
        <&> tagP "PairDelimiter"
        &> valueP
        @> (\a -> a)
\end{code}

\begin{code}
valueP :: Parser JValue
valueP =
       jstringP
       @> (\n -> JString n )
       <|> tagP "float"
       @> (\(_,n,_) -> JNumber (read n))
       <|> arrayP
       @> (\ n-> JArray (concat n))
       <|> objectP
       @> (\ n-> JObject (concat n))
\end{code}

\begin{code}
arrayP :: Parser [[JValue]]
arrayP = 
    tagP "StartOfArray"
    <&> optional( valueP 
        <&> many( tagP "AnotherValue"
        &> valueP
        )
        @> cons
    )
    <& nofail (tagP "EndOfArray" )
    @> (\((_, _, _), a ) -> a) 
\end{code}


\begin{code}
objectP :: Parser [[(String, JValue)]]
objectP = 
    tagP "StartOfObject"
    <&> optional( keyValP
    <&> many( tagP "AnotherValue"
        &> keyValP
        )
        @> cons
    )
    <& nofail (tagP "EndOfObject" )
    @> (\(_, a ) -> a) 
\end{code}


\begin{code}
programP :: Parser JSON
programP = nofail $ total (
    valueP @> JValue
   )
\end{code}



\subsection{Main function} %%%%%%%%%%%%%%%%%%%%%%%%

\begin{code}
main::IO()
main = do
    [f] <- getArgs
    fileData <- readFile f
    let error :: Pos -> Msg -> IO ()
        error (_,col) msg = do
            putStrLn $ "Error: " ++ msg
            putStrLn fileData
            let col' = if col < 0
                then length fileData
                else col
            putStrLn $ replicate col' ' ' ++ "^"
        cps = preLex fileData
    putStrLn $ "Pairs: " ++ show cps
    case programL cps of
      Error pos msg -> error pos msg
      OK (tlps,_) -> do
         putStrLn $ "Lexemes: " ++ show tlps
         case programP tlps of
            Error pos msg -> error pos msg
            OK (fd,_) -> do
               putStrLn $ "File Data : " ++ show fd
               case fd of
                  JValue e -> do
                     putStrLn $ "Result: " ++ show e
\end{code}