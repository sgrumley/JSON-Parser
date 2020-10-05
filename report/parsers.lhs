{\tt parser}
Parse String
\begin{code}
jstringP :: Parser String
jstringP = tagP "string"
    @> (\(_,s,_) -> s)
\end{code}

Parse Key : Value
\begin{code}
keyValP :: Parser (String, JValue)
keyValP = jstringP            --tagP "string"
        <&> tagP "PairDelimiter"
        &> valueP
        @> (\a -> a)
\end{code}

Parse Any value
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
       <|> tagP "true"
       @> (\(_,_,_) -> JBool True)
       <|> tagP "false"
       @> (\(_,_,_) -> JBool False)
\end{code}

Parse array
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

Parse JSON Object
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