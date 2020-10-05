module Main (main) where

import System.IO

import ABR.Util.Pos
import ABR.Parser
import ABR.Parser.Lexers

data Expr =   Number Double
            | Expr :+ Expr
            | Expr :- Expr
            | Expr :* Expr
            | Expr :/ Expr
            | Negate Expr 
            deriving Show

-- infixl 7 :*, :/
-- infixl 6 :+, :-



eval :: Expr -> Double
eval e = case e of
   Number x -> x
   e :+ e'  -> eval e + eval e'
   e :- e'  -> eval e - eval e'
   e :* e'  -> eval e * eval e'
   e :/ e'  -> eval e / eval e'
   Negate e -> negate (eval e)



data Command =   Quit
               | Expr Expr
               deriving Show



symbolL :: Lexer
symbolL = literalL '(' <|> literalL ')' <|> literalL '+' <|>
          literalL '-' <|> literalL '*' <|> literalL '/'

commandL :: Lexer
commandL = dropWhite $ nofail $ total $ listL 
   [whitespaceL,  literalL 'q', symbolL, floatL]



commandP :: Parser Command
commandP = nofail $ total (
          literalP "'q'" "q" #> Quit
      <|> exprP @> Expr
   )



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


sfactP :: Parser Expr
sfactP = 
       factorP
   <|> literalP "'-'" "-" &> sfactP @> Negate

factorP :: Parser Expr
factorP =
       tagP "float"
       @> (\(_,n,_) -> Number (read n))
   <|> literalP "'('" "("
       &> nofail' "expression expected" exprP
       <& nofail (literalP "')'" ")")

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
