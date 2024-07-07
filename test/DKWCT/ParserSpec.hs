{-# LANGUAGE OverloadedStrings #-}
module DKWCT.ParserSpec (spec) where

import qualified Data.Text as T
import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec
import DKWCT.Parser

spec :: Spec
spec = do
    describe "(final) expression" $ do
        it "can parse flat expression" $ do
            parse dModule "" "12 17 add 11 true if 17.5 't' \"hello world\"" `shouldParse` Module [] (Expression [DInteger 12, DInteger 17, DApplication "add", DInteger 11, DBoolean True, DApplication "if", DFloat 17.5, DChar 't', DString "hello world"])
            parse dModule "" "12 17 +  29 =" `shouldParse` Module [] (Expression [DInteger 12, DInteger 17, DApplication "+", DInteger 29, DApplication "="])
        it "can parse expression with layer" $ do
            parse dModule "" "[12 17 add 11 16] {}" `shouldParse` Module [] (Expression [DList 0 (Expression [DInteger 12, DInteger 17, DApplication "add", DInteger 11, DInteger 16]), DFunction 0 (Expression [])])
            parse dModule "" "(2: 19 0 87 [10 9 'z'] {5: [9: ()]})" `shouldParse` Module [] (Expression [DTuple 2 (Expression [DInteger 19, DInteger 0, DInteger 87, DList 0 (Expression [DInteger 10, DInteger 9, DChar 'z']), DFunction 5 (Expression [DList 9 (Expression [DTuple 0 (Expression [])])])])])
    describe "comments" $ do
        it "can parse line comments" $ do
            parse dModule "" (T.unlines ["lorem This is invalid code hopefully ( ] &*&*ghqrieofa ..,=====", "1"]) `shouldParse` Module [] (Expression [DInteger 1])
            parse dModule "" (T.unlines ["lorem lorem lorem can be repeated lorem", "1"]) `shouldParse` Module [] (Expression [DInteger 1])
            parse dModule "" (T.unlines ["lorem works when ; in line", "1"]) `shouldParse` Module [] (Expression [DInteger 1])
            parse dModule "" "1 lorem should work at the end of a line" `shouldParse` Module [] (Expression [DInteger 1])
        it "can parse multi-line comments" $ do
            parse dModule "" (T.unlines ["ipsum multiline", "comments", "work )(", "then dolor", "1"]) `shouldParse` Module [] (Expression [DInteger 1])
            parse dModule "" (T.unlines ["ipsum", "multiline", "comments", "work )(", "On their own lines", "dolor", "1"]) `shouldParse` Module [] (Expression [DInteger 1])
            parse dModule "" "1 ipsum also work in the middle of a line dolor" `shouldParse` Module [] (Expression [DInteger 1])
            parse dModule "" "1 ipsum also work in the middle of a line and parse afterwards dolor 9" `shouldParse` Module [] (Expression [DInteger 1, DInteger 9])
            parse dModule "" (T.unlines ["ipsum", "both at once work", "dolor", "1 ipsum something something dolor7"]) `shouldParse` Module [] (Expression [DInteger 1, DInteger 7])
        it "will fail when comments not closed" $ do
            parse dModule "" `shouldFailOn` T.unlines ["ipsum something", "something something not closed", "1 7"]
            parse dModule "" `shouldFailOn` T.unlines ["ipsum something", "something something not dolo", "1 7"]
            parse dModule "" `shouldFailOn` "1 ipsum also fails here"
        it "works properly when comments nested" $ do
            parse dModule "" (T.unlines ["lorem ipsum this should work", "1"]) `shouldParse` Module [] (Expression [DInteger 1])
            parse dModule "" (T.unlines ["lorem is not stopped by dolor", "1"]) `shouldParse` Module [] (Expression [DInteger 1])
            parse dModule "" (T.unlines ["ipsum", "multiline", "ipsum should be ignored", "work )(", "On their own lines", "dolor", "1"]) `shouldParse` Module [] (Expression [DInteger 1])
            parse dModule "" `shouldFailOn` T.unlines ["ipsum here", "something something dolor", ")(won't work with another dolor", "1"]
            parse dModule "" `shouldFailOn` T.unlines ["ipsum here", "adding another ipsum changes nothing", "something something dolor", ")(won't work with another dolor", "1"]
    describe "declarations" $ do
        it "can parse value declarations" $ do
            parse dModule "" (T.unlines ["const succ = 1 +;", "7 succ"]) `shouldParse` Module [ValueDeclaration "succ" (Expression [DInteger 1, DApplication "+"])] (Expression [DInteger 7, DApplication "succ"])
            parse dModule "" (T.unlines ["const succ = 1 +;", "const pred = 1 -;", "7 succ pred"]) `shouldParse` Module [ValueDeclaration "succ" (Expression [DInteger 1, DApplication "+"]), ValueDeclaration "pred" (Expression [DInteger 1, DApplication "-"])] (Expression [DInteger 7, DApplication "succ", DApplication "pred"])
            parse dModule "" "const succ = 1 +;7 succ" `shouldParse` Module [ValueDeclaration "succ" (Expression [DInteger 1, DApplication "+"])] (Expression [DInteger 7, DApplication "succ"])
            parse dModule "" "const succ = 1 +; 7 succ" `shouldParse` Module [ValueDeclaration "succ" (Expression [DInteger 1, DApplication "+"])] (Expression [DInteger 7, DApplication "succ"])
            parse dModule "" "const succ = 1 +;const pred = 1 -;7 succ pred" `shouldParse` Module [ValueDeclaration "succ" (Expression [DInteger 1, DApplication "+"]), ValueDeclaration "pred" (Expression [DInteger 1, DApplication "-"])] (Expression [DInteger 7, DApplication "succ", DApplication "pred"])
        it "can parse type declarations" $ do
            parse dModule "" (T.unlines ["typeOf one = Integer;", "one"]) `shouldParse` Module [TypeDeclaration "one" (TypeExpression [DType "Integer"])] (Expression [DApplication "one"])
            parse dModule "" (T.unlines ["typeOf one = Integer;", "typeOf nums = (Integer Integer);", "nums"]) `shouldParse` Module [TypeDeclaration "one" (TypeExpression [DType "Integer"]), TypeDeclaration "nums" (TypeExpression [DTypeTuple 0 (TypeExpression [DType "Integer", DType "Integer"])])] (Expression [DApplication "nums"])
            parse dModule "" "typeOf one = Integer;typeOf nums = (Integer Integer);nums" `shouldParse` Module [TypeDeclaration "one" (TypeExpression [DType "Integer"]), TypeDeclaration "nums" (TypeExpression [DTypeTuple 0 (TypeExpression [DType "Integer", DType "Integer"])])] (Expression [DApplication "nums"])
        it "can parse both" $ do
            parse dModule "" (T.unlines ["typeOf one = Integer;", "const one = 1;", "one"]) `shouldParse` Module [TypeDeclaration "one" (TypeExpression [DType "Integer"]), ValueDeclaration "one" (Expression [DInteger 1])] (Expression [DApplication "one"])
        it "fails without semicolon" $ do
            parse dModule "" `shouldFailOn` T.unlines ["const succ = 1 +", "const pred = 1 -", "7 succ pred"]
            parse dModule "" `shouldFailOn` T.unlines ["typeOf one = Integer", "typeOf nums = (Integer, Integer)", "nums"]
