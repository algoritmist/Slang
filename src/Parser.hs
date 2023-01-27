module Parser where

import qualified Data.Map as Map
import Language
import Text.Parsec.Expr
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Token (comma, lexeme)
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

unOp op = Prefix $ EUnOp op <$ mapValueBetweenSpaces unaryOperations op

binOp op = Infix (EBinOp op <$ mapValueBetweenSpaces binaryOperations op) AssocRight

operations =
  [ [unOp Not, unOp Neg],
    [binOp Mul, binOp Div],
    [binOp Sum, binOp Sub],
    [binOp GE, binOp LE, binOp L, binOp G],
    [binOp Eq, binOp NotE],
    [binOp And],
    [binOp Or]
  ]

subExpression :: Parser CoreExpr
subExpression =
  parens expression
    <|> letExpression
    <|> caseExpression
    <|> try application
    <|> variable
    <|> ENum <$> int

expression :: Parser CoreExpr
expression = buildExpressionParser operations subExpression

definition :: Parser CoreDefinition
definition = do
  name <- function
  whiteSpace
  vars <- variableList
  whiteSpace
  char '='
  whiteSpace
  expr <- expression
  char ';'
  return (name, vars, expr)

letExpression :: Parser CoreExpr
letExpression = do
  string "let"
  whiteSpace
  var <- variableDefinition
  whiteSpace
  string "in"
  whiteSpace
  expr <- expression
  return (ELet var expr)

caseExpression :: Parser CoreExpr
caseExpression = do
  string "case"
  whiteSpace
  expr <- expression
  whiteSpace
  string "of"
  whiteSpace
  alts <- alters
  return (ECase expr alts)

alters :: Parser [CoreAlter]
alters = many1 $ whiteSpace *> alter

alter :: Parser CoreAlter
alter = do
  string "option"
  whiteSpace
  tag <- int
  whiteSpace
  string "->"
  whiteSpace
  expr <- expression
  optionMaybe $ char ','
  return (Alter (tag, expr))

function :: Parser CoreFunction
function = identifier

variable :: Parser CoreExpr
variable = EVar <$> identifier

variableList :: Parser [CoreExpr]
variableList = try (many1 variable) <|> return []

variableAndFunctionList :: Parser [CoreExpr]
variableAndFunctionList = many1 (variable <|> try (ENum <$> int) <|> try (parens expression))

variableDefinition :: Parser CoreVarDefinition
variableDefinition = do
  name <- variable
  whiteSpace
  char '='
  whiteSpace
  expr <- expression
  return (name, expr)

int :: Parser Int
int = fromInteger <$> Token.integer lexer

application :: Parser CoreExpr
application = do
  f <- function
  vars <- variableAndFunctionList
  return $ EFunCall f vars

program :: Parser CoreProgram
program = many1 $ whiteSpace *> definition
