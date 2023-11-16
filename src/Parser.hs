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


int :: Parser Int
int = fromInteger <$> Token.integer lexer
float :: Parser Double
float = Token.float lexer -- parses a floating point value
bool :: Parser Bool
bool = False <$ string "false" <|> True <$ string "true"

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
    <|> ifExpression
    <|> letExpression
    <|> caseExpression
    <|> try application
    <|> try variableDefinition
    <|> try variable
    <|> primitive

expression :: Parser CoreExpr
expression = buildExpressionParser operations subExpression

definition :: Parser CoreExpr
definition = do
  name <- function
  whiteSpace
  vars <- variableList
  whiteSpace
  char '='
  whiteSpace
  expr <- expression
  char ';'
  return $ EDefinition $ Function name vars expr

letExpression :: Parser CoreExpr
letExpression = do
  string "let"
  whiteSpace
  exprs <- many1 variableDefinition
  whiteSpace
  string "in"
  whiteSpace
  var <- expression
  return (ELet exprs var)

ifExpression :: Parser CoreExpr
ifExpression = do
  string "if"
  whiteSpace
  condition <- expression
  whiteSpace
  string "then"
  whiteSpace
  trueBranch <- expression
  whiteSpace
  string "else"
  whiteSpace
  falseBranch <- variable
  return $ EIf condition (trueBranch, falseBranch)

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
alters = many1 $ whiteSpace *> alter <* whiteSpace

alter :: Parser CoreAlter
alter = do
  whiteSpace
  tag <- EOtherwise <$ string "otherwise" <|> primitive
  whiteSpace
  string "->"
  whiteSpace
  expr <- expression
  return (tag, expr)

function :: Parser String
function = identifier

variable :: Parser CoreExpr
variable = EVar <$> identifier

variableList :: Parser [CoreExpr]
variableList = try (many1 variable) <|> return []

variableAndFunctionList :: Parser [CoreExpr]
variableAndFunctionList = many1 (variable <|> try primitive <|> try subExpression)

variableDefinition :: Parser CoreExpr
variableDefinition = do
  name <- function
  whiteSpace
  char '='
  whiteSpace
  expr <- expression
  return $ EDefinition $ Function name [] expr

application :: Parser CoreExpr
application = do
  f <- function
  vars <- variableAndFunctionList
  return $ EFunCall f vars

program :: Parser CoreProgram
program = many1 $ whiteSpace *> definition <* whiteSpace

parseString :: Parser String
parseString = do
  char '"'
  s <- identifier
  char '"'
  return s

primitive :: Parser CoreExpr
primitive = EBool <$> bool <|> (EString <$> parseString) <|> try (EFloat <$> float) <|> try (EInt <$> int)
