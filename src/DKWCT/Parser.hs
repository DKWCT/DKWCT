module DKWCT.Parser (dModule, ExpressionElement(..), Expression(..), TypeExpressionElement(..), TypeExpression(..), Declaration(..), Module(..)) where

import DKWCT.Lexer (Parser)
import qualified DKWCT.Lexer as L
import Data.Scientific (floatingOrInteger)
import Data.Text (Text, pack)
import Text.Megaparsec
import Data.Maybe (fromMaybe)

data ExpressionElement = DFloat !Double | DInteger !Integer | DBoolean !Bool | DString !Text | DChar !Char | DList !Integer !Expression | DTuple !Integer !Expression | DFunction !Integer !Expression | DApplication !Text deriving (Show, Eq, Ord)
newtype Expression = Expression [ExpressionElement] deriving (Show, Eq, Ord)
data TypeExpressionElement = DType !Text | DTypeTuple !Integer !TypeExpression deriving (Show, Eq, Ord)
newtype TypeExpression = TypeExpression [TypeExpressionElement] deriving (Show, Eq, Ord)
data Declaration = ValueDeclaration !Text !Expression | TypeDeclaration !Text !TypeExpression deriving (Show, Eq, Ord)
data Module = Module ![Declaration] !Expression deriving (Show, Eq, Ord)

number :: Parser ExpressionElement
number = convert . floatingOrInteger <$> L.number
  where
    convert :: Either Double Integer -> ExpressionElement
    convert (Left d) = DFloat d
    convert (Right n) = DInteger n

boolean :: Parser ExpressionElement
boolean = DBoolean <$> L.boolean

string :: Parser ExpressionElement
string = DString . pack <$> L.str

char :: Parser ExpressionElement
char = DChar <$> L.character

list :: Parser ExpressionElement
list = L.brackets $ do
    inp <- optional $ try $ L.int <* L.colon
    DList (fromMaybe 0 inp) <$> expression

tuple :: Parser ExpressionElement
tuple = L.parens $ do
    inp <- optional $ try $ L.int <* L.colon
    DTuple (fromMaybe 0 inp) <$> expression

function :: Parser ExpressionElement
function = L.braces $ do
    inp <- optional $ try $ L.int <* L.colon
    DFunction (fromMaybe 0 inp) <$> expression

application :: Parser ExpressionElement
application = DApplication <$> L.identifier

expressionElement :: Parser ExpressionElement
expressionElement = try number <|> try boolean <|> string <|> char <|> list <|> tuple <|> function <|> application

expression1 :: Parser Expression
expression1 = Expression <$> some expressionElement

expression :: Parser Expression
expression = Expression <$> many expressionElement

dType :: Parser TypeExpressionElement
dType = DType <$> L.identifier

typeTuple :: Parser TypeExpressionElement
typeTuple = L.parens $ do
    inp <- optional $ try $ L.int <* L.colon
    DTypeTuple (fromMaybe 0 inp) <$> typeExpression

typeExpressionElement :: Parser TypeExpressionElement
typeExpressionElement = typeTuple <|> dType

typeExpression1 :: Parser TypeExpression
typeExpression1 = TypeExpression . reverse <$> some typeExpressionElement

typeExpression :: Parser TypeExpression
typeExpression = TypeExpression . reverse <$> many typeExpressionElement

valueDeclaration :: Parser Declaration
valueDeclaration = do
    L.constant
    name <- L.identifier
    L.equals
    ValueDeclaration name <$> expression1

typeDeclaration :: Parser Declaration
typeDeclaration = do
    L.typeOf
    name <- L.identifier
    L.equals
    TypeDeclaration name <$> typeExpression1

declaration :: Parser Declaration
declaration = valueDeclaration <|> typeDeclaration

dModule :: Parser Module
dModule = do
    L.start
    declarations <- endBy declaration L.semicolon
    expr <- expression1
    Module declarations expr <$ eof
