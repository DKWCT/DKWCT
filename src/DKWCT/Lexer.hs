{-# LANGUAGE OverloadedStrings #-}
module DKWCT.Lexer (Parser, int, number, str, character, boolean, parens, brackets, braces, semicolon, colon, equals, constant, typeOf, identifier, start) where

import Data.Scientific ( Scientific )
import Data.Text (Text, cons)
import Data.Char ( isDigit, isPrint, isSpace )
import Data.Functor ( ($>) )
import Text.Megaparsec
import Text.Megaparsec.Char ( char )
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void ( Void )
import Data.Set (Set)
import qualified Data.Set as S

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
brackets = between (symbol "[") (symbol "]")
braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")

colon :: Parser ()
colon = symbol ":" $> ()
semicolon :: Parser ()
semicolon = symbol ";" $> ()
equals :: Parser ()
equals = symbol "=" $> ()
constant :: Parser ()
constant = symbol "const " $> ()
typeOf :: Parser ()
typeOf = symbol "typeOf " $> ()

identifier :: Parser Text
identifier = lexeme identifier'
    where
        identifier' = cons <$> satisfy (testAll [isPrint, not . isSpace, not . isDigit, not . flip S.member reserved]) <*> takeWhileP Nothing (testAll [isPrint, not . isSpace, not . flip S.member reserved]) <?> "identifier"
        reserved :: Set Char
        reserved = S.fromList ['(', ')', '[', ']', '{', '}']
        testAll :: [a -> Bool] -> a -> Bool
        testAll = (and .) . sequence

start :: Parser ()
start = lexeme $ pure ()
