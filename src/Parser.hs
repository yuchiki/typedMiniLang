module Parser(parseExpr) where

import Expr
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
import qualified Text.Parsec.Token as P
import Text.Parsec.Language(emptyDef)

parseExpr :: String -> Either String Expr
parseExpr s =
    case parse mainParser "" s of
        Left err -> Left $ show err
        Right e -> Right e

mainParser :: Parser Expr
mainParser = expr

expr :: Parser Expr
expr =
    simpleExpr <|>
    ifExpr <|>
    letExpr <|>
    absExpr

ifExpr :: Parser Expr
ifExpr = do
    reserved "if"
    e1 <- expr
    reserved "then"
    e2 <- expr
    reserved "else"
    EIf e1 e2 <$> expr

letExpr :: Parser Expr
letExpr = do
    reserved "let"
    x <- identifier
    reservedOp "="
    e1 <- expr
    reservedOp "in"
    ELet x e1 <$> expr

absExpr :: Parser Expr
absExpr = do
    reserved "fun"
    x <- identifier
    reservedOp "->"
    EAbs x <$> expr


simpleExpr :: Parser Expr
simpleExpr = buildExpressionParser table term <?> "simple expression"
    where
        table = [
            [assocL "" EApp],
            [unary "!" ENot],
            [assocL "*" EMul, assocL "/" EDiv],
            [assocL "+" EAdd, assocL "-" ESub],
            [assocL "==" EEqInt, assocL "<" ELT],
            [assocL "&&" EAnd],
            [assocL "||" EOr]
            ]
        assocL s op = binop s op AssocLeft
        assocR s op = binop s op AssocRight
        binop s op = Infix (do { reservedOp s; return op} <?> s )
        unary s op = Prefix (do{ reservedOp s; return op })

term :: Parser Expr
term = variable <|> int <|> bool <|> parens expr

variable :: Parser Expr
variable = EVar <$> identifier

int :: Parser Expr
int = EInt . fromInteger <$> integer

bool :: Parser Expr
bool = do
        reserved "true"
        return (EBool True)
    <|> do
        reserved "false"
        return (EBool False)

def = emptyDef {
    P.reservedOpNames = ["+", "-", "*", "/" , "||", "!", "<", "==", "=", "->"],
    P.reservedNames = ["true", "false", "if", "then", "else", "let", "in", "fun"]
}

lexer = P.makeTokenParser def

integer = P.integer lexer
parens = P.parens lexer
identifier = P.identifier lexer
reservedOp = P.reservedOp lexer
reserved = P.reserved lexer
