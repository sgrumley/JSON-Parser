-- BFG interpreter

-- use: bfg script

module Main (main) where

import System.Environment
import System.IO
import qualified Data.Map as M
import Data.Char

import ABR.Util.Pos
import ABR.Parser
import ABR.Parser.Lexers

type Identifier = String

-- Variable Table

type VarTable = M.Map Identifier Double

-- Expressions

-- This type represents an expression.

data Expr =   Number Double
            | Ident Identifier
            | Expr :+ Expr
            | Expr :- Expr
            | Expr :* Expr
            | Expr :/ Expr
            | Negate Expr 
   deriving Show

infixl 7 :*, :/
infixl 6 :+, :-

-- Evaluating an expression

eval :: VarTable -> Expr -> Double
eval vt e = case e of
   Number x -> x
   Ident n -> case M.lookup n vt of
      Nothing -> error $ "variable" ++ n ++ " is undefined."
      Just d  -> d
   e :+ e'  -> eval vt e + eval vt e'
   e :- e'  -> eval vt e - eval vt e'
   e :* e'  -> eval vt e * eval vt e'
   e :/ e'  -> eval vt e / eval vt e'
   Negate e -> negate (eval vt e)

-- Lexer

identifierL :: Lexer
identifierL = 
        satisfyL isAlpha ""
   <&&> (many (     satisfyL isAlpha ""
                <|> satisfyL isDigit ""
                <|> literalL '_') &%> "")
   %> "identifier"

symbolL :: Lexer
symbolL = literalL '(' <|> literalL ')' <|> literalL '+' <|>
          literalL '-' <|> literalL '*' <|> literalL '/' <|>
          literalL '=' <|> literalL ';'

programL :: Lexer
programL = dropWhite $ nofail $ total $ listL 
   [whitespaceL, floatL, symbolL, identifierL]
   
-- Parsers

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
       tagP "identifier"
       @> (\(_,n,_) -> Ident n)
   <|> tagP "float"
       @> (\(_,n,_) -> Number (read n))
   <|> literalP "'('" "("
       &> nofail' "expression expected" exprP
       <& nofail (literalP "')'" ")")

data Assignment = Ass Identifier Expr
   deriving Show

assignmentP :: Parser Assignment
assignmentP =
       tagP "identifier"
   <&> literalP "'='" "="
    &> nofail' "expression expected" exprP
   <&  nofail' "semicolon expected" (literalP "';'" ";")
   @> (\((_,n,_), e) -> Ass n e)

data PrintStatement = Print Expr
   deriving Show

printStatementP :: Parser PrintStatement
printStatementP =
       literalP "identifier" "print"
    &> nofail' "expression expected" exprP
   <&  nofail' "semicolon expected" (literalP "';'" ";")
   @> Print

data Statement = AssStmt Assignment | PrintStmt PrintStatement
   deriving Show

statementP :: Parser Statement
statementP =
       assignmentP     @> AssStmt  
   <|> printStatementP @> PrintStmt
     
type Program = [Statement]

programP :: Parser Program
programP =
   nofail' "statement expected" $ total $ many statementP

main :: IO ()
main = do
   args <- getArgs
   case args of
      [path] -> interpret path
      _      -> error "wrong number of arguments"

run :: IO ()
run = interpret "add.bfg"

interpret :: FilePath -> IO ()
interpret path = do
   source <- readFile path
   putStrLn "----- source code ------"
   putStr source
   let cps = preLex source
   putStrLn "----- cps ------"
   print cps
   case programL cps of
      Error pos msg -> putStr $ errMsg pos msg source
      OK (tlps,_) -> do
         putStrLn "----- cps ------"
         print tlps
         case programP tlps of
            Error pos msg -> putStr $ errMsg pos msg source
            OK (prog,_)   -> do
               putStrLn "----- prog ------"
               print prog
               putStrLn "----- output ------"
               execute M.empty prog
               
execute :: VarTable -> Program -> IO ()
execute vt prog = case prog of
   []     -> return ()
   s : ss -> do
      case s of
         AssStmt (Ass n e)   -> do
            let d = eval vt e
                vt' = M.insert n d vt
            execute vt' ss
         PrintStmt (Print e) -> do
            let d = eval vt e
            print d
            execute vt ss
   
   
{-  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 



-- Main function

               case cmd of
                  Quit         -> 
                     return ()
                  Expr e -> do
                     putStrLn $ "Result: "
                        ++ show (eval e)
                     main
-}
