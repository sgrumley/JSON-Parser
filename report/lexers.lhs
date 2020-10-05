{\tt lexer} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
Lexer for boolean values
\begin{code}
boolL :: Lexer
boolL = tokenL "true" %> "true"
    <|> tokenL "false" %> "false"
\end{code}

Lexer for array type symbols
\begin{code}
arrayL :: Lexer
arrayL = literalL '[' %> "StartOfArray" 
    <|> literalL ']' %> "EndOfArray" 
\end{code}

Lexer for object type symbols
\begin{code}
objectL :: Lexer
objectL = literalL '{' %> "StartOfObject"
    <|> literalL '}' %> "EndOfObject"
    <|> literalL ':' %> "PairDelimiter" 
\end{code}

Lexer for more than one value symbols
\begin{code}
repeatableL :: Lexer
repeatableL = literalL ',' %> "AnotherValue"
\end{code}

\begin{code}
programL :: Lexer
programL = dropWhite $ nofail $ total $ listL 
    [whitespaceL, floatL, repeatableL, stringL, arrayL, objectL, boolL]
\end{code}