module Parser where

import qualified Data.Map as Map
import Language
import Syntax.Abstract
import Text.Parsec.Expr
import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as Token

lookupR :: Eq b => b -> Map.Map c b -> c
lookupR v = fst . head . Map.assocs . Map.filter (== v)

lexer = Token.makeTokenParser languageDef

whiteSpace = Token.whiteSpace lexer

identifier = Token.identifier lexer -- parses an identifier

parens = Token.parens lexer -- parses surrounding parenthesis:

braces = Token.braces lexer

commaSep = Token.commaSep lexer

float = Token.float lexer -- parses a floating point value

stringLiteral = Token.stringLiteral lexer -- parses a literal string

mapValueBetweenSpaces :: Eq a => Map.Map String a -> a -> Parser String
mapValueBetweenSpaces m v = try (whiteSpace *> string (lookupR v m) <* whiteSpace)

oneOfKeys :: Map.Map String a -> Parser a
oneOfKeys m = ((Map.!) m) <$> (choice . map string . Map.keys $ m)

unOp op = Prefix $ UnaryExpression op <$ mapValueBetweenSpaces unaryOperations op

binOp op = Infix (BinaryExpression op <$ mapValueBetweenSpaces binaryOperations op) AssocLeft

operations =
  [ [unOp Not, unOp Neg],
    [binOp Mul, binOp Div],
    [binOp Sum, binOp Sub],
    [binOp GE, binOp LE, binOp L, binOp G],
    [binOp Eq, binOp NotE],
    [binOp And],
    [binOp Or]
  ]

subExpression :: Parser Expression
subExpression =
  parens expression
    <|> ELet <$> letExpression
    <|> ECase <$> caseExpression
    <|> EVar <$> variable
    <|> ENum <$> int

expression :: Parser Expression
expression = buildExpressionParser operations subExpression

definition :: Parser CoreDefinition
definition = do
  name <- function
  whiteSpace
  variables <- variableList
  whiteSpace
  char '='
  whiteSpace
  expr <- expression
  return (CoreDefinition (name, variables, expr))

letExpression :: Parser ELet
letExpression = do
  string "let"
  whiteSpace
  variable <- variableDefinition
  whiteSpace
  string "in"
  whiteSpace
  return (ELet variable <$> varAssigns)

caseExpression :: Parser ECase
caseExpression = do
  string "case"
  whiteSpace
  variable <- variableDefinition
  whiteSpace
  string "of"
  return (ECase expr <$> alters)

alters :: Parser [CoreAlter]
alters = many1 alter

alter :: Parser CoreAlter
alter = do
  char "<"
  tag <- int
  char ">"
  whiteSpace
  string "->"
  whiteSpace
  expr <- expression
  return (CoreAlter (tag, expr))

function :: Parser Function

variable = Function <$> identifier

variable :: Parser EVar
variable = EVar <$> identifier

int :: Parser Int
int = fromInteger <$> Token.integer lexer
