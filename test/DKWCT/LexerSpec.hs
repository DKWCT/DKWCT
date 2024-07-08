{-# LANGUAGE OverloadedStrings #-}
module DKWCT.LexerSpec (spec) where

import DKWCT.Lexer
import Text.Megaparsec (parse, eof)
import Test.Hspec
import Test.Hspec.Megaparsec
import Data.Scientific

spec :: Spec
spec = do
    describe "int" $ do
        it "can parse integers" $ do
            parse (int <* eof) "" "1234" `shouldParse` 1234
            parse (int <* eof) "" "100132" `shouldParse` 100132
            parse (int <* eof) "" "00198" `shouldParse` 198
        it "can't parse floats" $ do
            parse (int <* eof) "" `shouldFailOn` "12.34"
            parse (int <* eof) "" `shouldFailOn` "9.0"
        it "can't parse sign'" $ do
            parse (int <* eof) "" `shouldFailOn` "+123"
            parse (int <* eof) "" `shouldFailOn` "-65"
    describe "number" $ do
        it "can parse integers" $ do
            parse (number <* eof) "" "1234" `shouldParse` scientific 1234 0
            parse (number <* eof) "" "1200" `shouldParse` scientific 12 2
            parse (number <* eof) "" "0014" `shouldParse` scientific 14 0
        it "can parse floats" $ do
            parse (number <* eof) "" "12.34" `shouldParse` scientific 1234 (-2)
            parse (number <* eof) "" "0.0012" `shouldParse` scientific 12 (-4)
            parse (number <* eof) "" "0.5200" `shouldParse` scientific 52 (-2)
            parse (number <* eof) "" "13.0" `shouldParse` scientific 13 0
        it "can parse sign" $ do
            parse (number <* eof) "" "+97" `shouldParse` scientific 97 0
            parse (number <* eof) "" "-85" `shouldParse` scientific (-85) 0
            parse (number <* eof) "" "+17.4" `shouldParse` scientific 174 (-1)
            parse (number <* eof) "" "-23.8" `shouldParse` scientific (-238) (-1)
        it "fails on just sign(s)" $ do
            parse (number <* eof) "" `shouldFailOn` "+"
            parse (number <* eof) "" `shouldFailOn` "-"
            parse (number <* eof) "" `shouldFailOn` "-+"
            parse (number <* eof) "" `shouldFailOn` "-+-"
        it "fails with multiple signs" $ do
            parse (number <* eof) "" `shouldFailOn` "+-17"
            parse (number <* eof) "" `shouldFailOn` "--++1"
            parse (number <* eof) "" `shouldFailOn` "--3"
        it "fails with spaces" $ do
            parse (number <* eof) "" `shouldFailOn` "+ 13"
            parse (number <* eof) "" `shouldFailOn` "- 76"
    describe "string" $ do
        it "can parse strings" $ do
            parse (str <* eof) "" "\"abcd\"" `shouldParse` "abcd"
            parse (str <* eof) "" "\"hello World\"" `shouldParse` "hello World"
        it "can parse special characters" $ do
            parse (str <* eof) "" "\"goodbye \\n there\"" `shouldParse` "goodbye \n there"
            parse (str <* eof) "" "\"this is \\t a tab\"" `shouldParse` "this is \t a tab"
        it "fails without quotes" $ do
            parse (str <* eof) "" `shouldFailOn` "hello"
    describe "character" $ do
        it "can parse strings" $ do
            parse (character <* eof) "" "'a'" `shouldParse` 'a'
            parse (character <* eof) "" "' '" `shouldParse` ' '
        it "can parse special characters" $ do
            parse (character <* eof) "" "'\\n'" `shouldParse` '\n'
            parse (character <* eof) "" "'\\t'" `shouldParse` '\t'
        it "fails without quotes" $ do
            parse (character <* eof) "" `shouldFailOn` "h"
    describe "boolean" $ do
        it "can parse 'true'" $ do
            parse (boolean <* eof) "" "true" `shouldParse` True
        it "can parse 'false'" $ do
            parse (boolean <* eof) "" "false" `shouldParse` False
        it "fails on other input" $ do
            parse (boolean <* eof) "" `shouldFailOn` "hello"
            parse (boolean <* eof) "" `shouldFailOn` "True"
            parse (boolean <* eof) "" `shouldFailOn` "False"
            parse (boolean <* eof) "" `shouldFailOn` "tralse"
            parse (boolean <* eof) "" `shouldFailOn` "talse"
            parse (boolean <* eof) "" `shouldFailOn` "frue"
    describe "parentheses" $ do
        it "can parse parentheses" $ do
            parse (parens int <* eof) "" "(12)" `shouldParse` 12
        it "fails without parentheses" $ do
            parse (parens int <* eof) "" `shouldFailOn` "12"
            parse (parens int <* eof) "" `shouldFailOn` "{12}"
    describe "brackets" $ do
        it "can parse brackets" $ do
            parse (brackets int <* eof) "" "[12]" `shouldParse` 12
        it "fails without brackets" $ do
            parse (brackets int <* eof) "" `shouldFailOn` "12"
            parse (brackets int <* eof) "" `shouldFailOn` "{12}"
    describe "braces" $ do
        it "can parse braces" $ do
            parse (braces int <* eof) "" "{12}" `shouldParse` 12
        it "fails without braces" $ do
            parse (braces int <* eof) "" `shouldFailOn` "12"
            parse (braces int <* eof) "" `shouldFailOn` "[12]"
    describe "semicolon" $ do
        it "can parse semicolon" $ do
            parse (semicolon <* eof) "" ";" `shouldParse` ()
        it "fails on other input" $ do
            parse (semicolon <* eof) "" `shouldFailOn` ":"
            parse (semicolon <* eof) "" `shouldFailOn` "semicolon"
    describe "colon" $ do
        it "can parse colon" $ do
            parse (colon <* eof) "" ":" `shouldParse` ()
        it "fails on other input" $ do
            parse (colon <* eof) "" `shouldFailOn` ";"
            parse (colon <* eof) "" `shouldFailOn` "colon"
    describe "equals" $ do
        it "can parse equals" $ do
            parse (equals <* eof) "" "=" `shouldParse` ()
        it "fails on other input" $ do
            parse (equals <* eof) "" `shouldFailOn` ":"
            parse (equals <* eof) "" `shouldFailOn` "equals"
    describe "constant" $ do
        it "can parse const" $ do
            parse (constant <* eof) "" "const " `shouldParse` ()
        it "fails without space" $ do
            parse (constant <* eof) "" `shouldFailOn` "const"
        it "fails on other input" $ do
            parse (constant <* eof) "" `shouldFailOn` "random stuff "
            parse (constant <* eof) "" `shouldFailOn` "CONST "
            parse (constant <* eof) "" `shouldFailOn` "konst "
    describe "typeOf" $ do
        it "can parse typeOf" $ do
            parse (typeOf <* eof) "" "typeOf " `shouldParse` ()
        it "fails without space" $ do
            parse (typeOf <* eof) "" `shouldFailOn` "typeOf"
        it "fails on other input" $ do
            parse (typeOf <* eof) "" `shouldFailOn` "rubbish here "
            parse (typeOf <* eof) "" `shouldFailOn` "typing "
            parse (typeOf <* eof) "" `shouldFailOn` "typeof "
    describe "identifier" $ do
        it "can parse identifiers" $ do
            parse (identifier <* eof) "" "hello" `shouldParse` "hello"
            parse (identifier <* eof) "" "anApple" `shouldParse` "anApple"
            parse (identifier <* eof) "" "anApple12" `shouldParse` "anApple12"
            parse (identifier <* eof) "" "AnApple" `shouldParse` "AnApple"
            parse (identifier <* eof) "" "+" `shouldParse` "+"
            parse (identifier <* eof) "" "π" `shouldParse` "π"
            parse (identifier <* eof) "" "∞" `shouldParse` "∞"
            parse (identifier <* eof) "" "÷" `shouldParse` "÷"
            parse (identifier <* eof) "" "√" `shouldParse` "√"
        it "can't parse space characters" $ do
            parse (identifier <* eof) "" `shouldFailOn` "an apple"
            parse (identifier <* eof) "" `shouldFailOn` "an\tapple"
            parse (identifier <* eof) "" `shouldFailOn` "an\napple"
        it "can't parse non-printable characters" $ do
            parse (identifier <* eof) "" `shouldFailOn` "\31"
            parse (identifier <* eof) "" `shouldFailOn` "null \0"
        it "can't start with a number" $ do
            parse (identifier <* eof) "" `shouldFailOn` "17hello"
        it "can't include parentheses-like characters" $ do
            parse (identifier <* eof) "" `shouldFailOn` ")he"
            parse (identifier <* eof) "" `shouldFailOn` "he]"
            parse (identifier <* eof) "" `shouldFailOn` "}the"
            parse (identifier <* eof) "" `shouldFailOn` "(abc"
            parse (identifier <* eof) "" `shouldFailOn` "[def"
            parse (identifier <* eof) "" `shouldFailOn` "{ghi"
            parse (identifier <* eof) "" `shouldFailOn` "(}{{}[])]][]"
    describe "start/space" $ do
        it "can parse spaced" $ do
            parse (start <* eof) "" " " `shouldParse` ()
            parse (start <* eof) "" "   " `shouldParse` ()
            parse (start <* eof) "" "\n" `shouldParse` ()
            parse (start <* eof) "" "  \t\n     " `shouldParse` ()
        it "can parse comments" $ do
            parse (start <* eof) "" "lorem this is a line comment" `shouldParse` ()
            parse (start <* eof) "" "ipsum this is a block comment dolor" `shouldParse` ()
            parse (start <* eof) "" "ipsum\n this is a \n block comment \n dolor" `shouldParse` ()
        it "can parse combinations" $ do
            parse (start <* eof) "" "  \n\t lorem some text here\n\t\t" `shouldParse` ()
            parse (start <* eof) "" " \n\t\tipsum some stuff here dolor \n\n\n lorem something here \n \t" `shouldParse` ()
        it "can parse empty" $ do
            parse (start <* eof) "" "" `shouldParse` ()
        it "fails on invalid input" $ do
            parse (start <* eof) "" `shouldFailOn` "hello there"
            parse (start <* eof) "" `shouldFailOn` "1729"
            parse (start <* eof) "" `shouldFailOn` ".?!"
            parse (start <* eof) "" `shouldFailOn` "\31\0"
        it "fails after leaving comment" $ do
            parse (start <* eof) "" `shouldFailOn` "lorem this is fine \n this is not"
            parse (start <* eof) "" `shouldFailOn` "ipsum a comment dolor not a comment"
            parse (start <* eof) "" `shouldFailOn` "ipsum\n a comment \ndolor\n not a comment"
