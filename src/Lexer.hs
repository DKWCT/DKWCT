{-# LANGUAGE OverloadedStrings #-}
module Lexer (int, number, str, character, boolean, parens, brackets, braces, colon, semicolon, comma, equals, constant, typeOf, identifier, start) where

import Data.Scientific ( Scientific )
import Data.Text (Text, cons)
import Data.Char
import Data.Functor ( ($>) )
import Text.Megaparsec
import Text.Megaparsec.Char hiding (space)
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void ( Void )

type Parser = Parsec Void Text

sc :: Parser ()
sc = takeWhile1P (Just "space") isSpace $> ()
lineComment :: Parser ()
lineComment = L.skipLineComment "lorem"
blockComment :: Parser ()
blockComment = L.skipBlockCommentNested "ipsum" "dolor"
space :: Parser ()
space = L.space sc lineComment blockComment
lexeme :: Parser a -> Parser a
lexeme = L.lexeme space
symbol :: Text -> Parser Text
symbol = L.symbol space

int :: Parser Integer
int = lexeme L.decimal
number :: Parser Scientific
number = L.signed (pure ()) number'
    where
        number' = lexeme L.scientific
str :: Parser String
str = lexeme $ char '"' *> manyTill L.charLiteral (char '"')
character :: Parser Char
character = lexeme $ char '\'' *> L.charLiteral <* char '\''
boolean :: Parser Bool
boolean = symbol "true" $> True <|> symbol "false" $> False

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")
brackets :: Parser a -> Parser a
brackets = between (symbol "(") (symbol ")")
braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")

colon :: Parser ()
colon = symbol ":" $> ()
semicolon :: Parser ()
semicolon = symbol ";" $> ()
comma :: Parser ()
comma = symbol "," $> ()
equals :: Parser ()
equals = symbol "=" $> ()
constant :: Parser ()
constant = spaceChar *> symbol "const " $> ()
typeOf :: Parser ()
typeOf = spaceChar *> symbol "typeOf " $> ()

testAll :: [a -> Bool] -> a -> Bool
testAll = (and .) . sequence
identifier :: Parser Text
identifier = lexeme identifier'
    where
        identifier' = cons <$> satisfy (testAll [isPrint, not . isSpace, not . isDigit]) <*> takeWhile1P Nothing (testAll [isPrint, not . isSpace]) <?> "identifier"
start :: Parser ()
start = lexeme $ pure ()
