module Main (main) where

import System.Environment
import ABR.Util.Pos
import ABR.Parser
import ABR.Parser.Lexers

------------------------------------------------ Data ------------------------------------------------

data JValue
    = JNumber Float
    | JString String
    | JArray[JValue]
    | JObject [(String, JValue)]
    | JBool Bool
    deriving(Show,Eq)

data JSON =  JValue JValue
               deriving Show

------------------------------------------------ Lexers ------------------------------------------------

-- Lexer for array type of symbols
arrayL :: Lexer
arrayL = literalL '[' %> "StartOfArray" 
    <|> literalL ']' %> "EndOfArray" 

-- Lexer for object type symbols
objectL :: Lexer
objectL = literalL '{' %> "StartOfObject"
    <|> literalL '}' %> "EndOfObject"
    <|> literalL ':' %> "PairDelimiter" 
    
-- Lexer for more than one value symbols
repeatableL :: Lexer
repeatableL = literalL ',' %> "AnotherValue"

-- Lexer for the program
programL :: Lexer
programL = dropWhite $ nofail $ total $ listL 
    [whitespaceL, floatL, repeatableL, stringL, arrayL, objectL]

------------------------------------------------ Parsers ------------------------------------------------

-- Parse String
jstringP :: Parser String
jstringP = tagP "string"
    @> (\(_,s,_) -> s)

-- Parse Key : Value
keyValP :: Parser (String, JValue)
keyValP = jstringP            --tagP "string"
        <&> tagP "PairDelimiter"
        &> valueP
        @> (\a -> a)

-- Parse Any value
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

-- Parse array
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

-- Parse JSON Object
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

-- Parse JSON
programP :: Parser JSON
programP = nofail $ total (
    valueP @> JValue
   )
------------------------------------------------ Driver ------------------------------------------------

-- Run Parser from file
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


