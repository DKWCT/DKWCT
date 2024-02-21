module DKWCT.Parser (dModule) where

import DKWCT.Lexer (Parser)
import qualified DKWCT.Lexer as L
import Data.Scientific (floatingOrInteger)
import Data.Text (Text, pack)
import Text.Megaparsec
import Data.Maybe (fromMaybe)

data ExpressionElement = DFloat !Double | DInteger !Integer | DBoolean !Bool | DString !Text | DChar !Char | DList !Integer !Integer !Expression | DTuple !Integer !Integer !Expression | DFunction !Integer !Integer !Expression | DApplication !Text
newtype Expression = Expression [ExpressionElement]
data TypeExpressionElement = DType !Text | DTypeTuple !Integer !Integer !TypeExpression
newtype TypeExpression = TypeExpression [TypeExpressionElement]
data Declaration = ValueDeclaration !Text !Expression | TypeDeclaration !Text !TypeExpression
data Module = Module ![Declaration] !Expression

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
    inp <- try $ optional $ L.int <* L.colon
    out <- try $ optional $ L.int <* L.colon
    DList (fromMaybe 0 inp) (fromMaybe 0 out) <$> expression

tuple :: Parser ExpressionElement
tuple = L.parens $ do
    inp <- try $ optional $ L.int <* L.colon
    out <- try $ optional $ L.int <* L.colon
    DTuple (fromMaybe 0 inp) (fromMaybe 0 out) <$> expression

function :: Parser ExpressionElement
function = L.braces $ do
    inp <- try $ optional $ L.int <* L.colon
    out <- try $ optional $ L.int <* L.colon
    DFunction (fromMaybe 0 inp) (fromMaybe 0 out) <$> expression

application :: Parser ExpressionElement
application = DApplication <$> L.identifier

expressionElement :: Parser ExpressionElement
expressionElement = try number <|> try boolean <|> string <|> char <|> list <|> tuple <|> function <|> application

expression :: Parser Expression
expression = Expression <$> some expressionElement

dType :: Parser TypeExpressionElement
dType = DType <$> L.identifier

typeTuple :: Parser TypeExpressionElement
typeTuple = L.parens $ do
    inp <- try $ optional $ L.int <* L.colon
    out <- try $ optional $ L.int <* L.colon
    DTypeTuple (fromMaybe 0 inp) (fromMaybe 0 out) <$> typeExpression

typeExpressionElement :: Parser TypeExpressionElement
typeExpressionElement = typeTuple <|> dType

typeExpression :: Parser TypeExpression
typeExpression = TypeExpression <$> some typeExpressionElement

valueDeclaration :: Parser Declaration
valueDeclaration = do
    L.constant
    name <- L.identifier
    L.equals
    ValueDeclaration name <$> expression

typeDeclaration :: Parser Declaration
typeDeclaration = do
    L.typeOf
    name <- L.identifier
    L.equals
    TypeDeclaration name <$> typeExpression

declaration :: Parser Declaration
declaration = valueDeclaration <|> typeDeclaration

dModule :: Parser Module
dModule = do
    L.start
    declarations <- endBy declaration L.semicolon
    Module declarations <$> expression
