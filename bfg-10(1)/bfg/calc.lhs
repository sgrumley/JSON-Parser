\module{calc} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

The program {\tt calc} is an algebraic calculator.

\begin{code}
module Main (main) where
\end{code}

\begin{code}
import System.IO
\end{code}

\begin{code}
import ABR.Util.Pos
import ABR.Parser
import ABR.Parser.Lexers
\end{code}

\SPAGE %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\submodule{Expressions} %%%%%%%%%%%%%%%%%%%%%%%%

This type represents an expression.

\begin{code}
data Expr =   Number Double
            | Expr :+ Expr
            | Expr :- Expr
            | Expr :* Expr
            | Expr :/ Expr
            | Negate Expr 
            deriving Show
\end{code}

\begin{code}
infixl 7 :*, :/
infixl 6 :+, :-
\end{code}

\SPAGE %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\submodule{Evaluating an expression} %%%%%%%%%%%%%%%%%%%%%%%%

\begin{code}
eval :: Expr -> Double
eval e = case e of
   Number x -> x
   e :+ e'  -> eval e + eval e'
   e :- e'  -> eval e - eval e'
   e :* e'  -> eval e * eval e'
   e :/ e'  -> eval e / eval e'
   Negate e -> negate (eval e)
\end{code}

\SPAGE %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\submodule{Commands} %%%%%%%%%%%%%%%%%%%%%%%%

This type represents the commands available to
the user. The user types {\tt q} to quit or an expression to
evaluate.

\begin{code}
data Command =   Quit
               | Expr Expr
               deriving Show
\end{code}

\SPAGE %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\submodule{Lexer} %%%%%%%%%%%%%%%%%%%%%%%%

\InputEBNF{calc}{symbol}

\SPAGE %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\begin{code}
symbolL :: Lexer
symbolL = literalL '(' <|> literalL ')' <|> literalL '+' <|>
          literalL '-' <|> literalL '*' <|> literalL '/'
\end{code}

\InputEBNF{calc}{commandL}

\SPAGE %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\begin{code}
commandL :: Lexer
commandL = dropWhite $ nofail $ total $ listL 
   [whitespaceL,  literalL 'q', symbolL, floatL]
\end{code}

\SPAGE %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\submodule{Parsers} %%%%%%%%%%%%%%%%%%%%%%%%

\InputEBNF{calc}{command}

\begin{code}
commandP :: Parser Command
commandP = nofail $ total (
          literalP "'q'" "q" #> Quit
      <|> exprP @> Expr
   )
\end{code}

\SPAGE %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\InputEBNF{calc}{expr}

Note that these operators are left-associative. This leads to
left-recursive EBNF productions. Parsers based on left-recursive
productions always loop endlessly, so we rewrite them as follows.

\begin{ebnf}
expr  ::= term expr'
expr' ::= ("+" | "-") term expr' | epsilon
\end{ebnf}

\SPAGE %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\begin{code}
exprP :: Parser Expr
exprP = termP <&> expr'P @> (\(t,e') -> e' t)
   where
   expr'P :: Parser (Expr -> Expr)
   expr'P = 
          (literalP "'+'" "+" 
             <|> literalP "'-'" "-")
          <&> nofail' "term expected" 
             (termP <&> expr'P ) 
          @> (\ ((_,o,_),(t,e')) -> case o of
                "+" -> e' . (:+ t)
                "-" -> e' . (:- t)
            )
      <|> succeedA id
\end{code}

\SPAGE %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\InputEBNF{calc}{term}

This must be rewritten for the same reason. 

\begin{ebnf}
term  ::= sfact term'
term' ::=   ("*" | "/") sfact term' | epsilon
\end{ebnf}

\SPAGE %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\begin{code}
termP :: Parser Expr
termP = sfactP <&> term'P @> (\(sf,t') -> t' sf)
   where
   term'P :: Parser (Expr -> Expr)
   term'P =
          (literalP "'*'" "*" 
              <|> literalP "'/'" "/")
          <&> nofail' "signed factor expected"
              (sfactP <&> term'P ) 
          @> (\ ((_,o,_),(sf,t')) -> case o of
                "*" -> t' . (:* sf)
                "/" -> t' . (:/ sf)
             )
      <|> succeedA id
\end{code}

\SPAGE %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\InputEBNF{calc}{sfact}

\begin{code}
sfactP :: Parser Expr
sfactP = 
       factorP
   <|> literalP "'-'" "-" &> sfactP @> Negate
\end{code}

\SPAGE %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\InputEBNF{calc}{factor}

\begin{code}
factorP :: Parser Expr
factorP =
       tagP "float"
       @> (\(_,n,_) -> Number (read n))
   <|> literalP "'('" "("
       &> nofail' "expression expected" exprP
       <& nofail (literalP "')'" ")")
\end{code}

\SPAGE %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\submodule{Main function} %%%%%%%%%%%%%%%%%%%%%%%%

\begin{code}
main :: IO ()
main = do
   putStr "? "
   hFlush stdout
   command <- getLine
   let error :: Pos -> Msg -> IO ()
       error (_,col) msg = do
          putStrLn $ "Error: " ++ msg
          putStrLn command
          let col' = if col < 0
                 then length command
                 else col
          putStrLn $ replicate col' ' '
             ++ "^"
          main
       cps = preLex command
   putStrLn $ "Pairs: " ++ show cps
   case commandL cps of
      Error pos msg -> error pos msg
      OK (tlps,_)      -> do
         putStrLn $ "Lexemes: " ++ show tlps
         case commandP tlps of
            Error pos msg -> error pos msg
            OK (cmd,_)    -> do
               putStrLn $ "Command : " 
                  ++ show cmd
               case cmd of
                  Quit         -> 
                     return ()
                  Expr e -> do
                     putStrLn $ "Result: "
                        ++ show (eval e)
                     main
\end{code}
