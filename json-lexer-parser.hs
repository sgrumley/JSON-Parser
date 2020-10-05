-- https://stackoverflow.com/questions/24386664/construction-of-json-types-in-haskell
-- http://hackage.haskell.org/package/aeson-1.5.4.0/docs/src/Data.Aeson.Types.Internal.html#Value
--http://hackage.haskell.org/package/microaeson-0.1.0.0/docs/src/Data.Aeson.Micro.html#Value
--https://www.reddit.com/r/haskell/comments/iyuxcc/implement_haskell_data_types_for_json_data_is/
--http://book.realworldhaskell.org/read/writing-a-library-working-with-json-data.html
--https://github.com/noughtmare/uuagc/blob/mirage/uuagc/trunk/src/JSON.hs

--http://docshare02.docshare.tips/files/27095/270958091.pdf


module Main (main) where

import System.IO
import Data.Char

import ABR.Util.Pos
import ABR.Parser
import ABR.Parser.Lexers

data JValue
    = JNumber Float
    | JString String
    | JArray[JValue]
    | JObject [(String, JValue)]
    | JBool Bool
    deriving(Show,Eq)

data JSON =  JValue JValue
               deriving Show


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

-- Main
main :: IO ()
main = do
   putStr "$ "
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
   case programL cps of
      Error pos msg -> error pos msg
      OK (tlps,_)      -> do
         putStrLn $ "Lexemes: " ++ show tlps
         case programP tlps of
            Error pos msg -> error pos msg
            OK (cmd,_)    -> do
               putStrLn $ "Command : " 
                  ++ show cmd
               case cmd of
                  JValue e -> do
                     putStrLn $ "Result: "
                        ++ show e
                     main