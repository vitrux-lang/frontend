module Language.Mill.Parse where

import Control.Applicative ((<$>), (<|>), (<*), (<*>), (*>))
import Control.Monad (foldM)
import Text.Parsec (eof, sepBy, sepBy1, try, sepEndBy, many, getState, modifyState)
import Language.Mill.Lex
import Language.Mill.AST

newID :: ID a => Parser a
newID = do
    id <- idFromInt <$> getState
    modifyState succ
    return id

module_ :: Parser Module
module_ = Module <$> many decl <* eof

name :: Parser Name
name = do
    parts <- identifier `sepBy1` dot
    return $ case parts of
        [id] -> UnqualifiedName id
        xs -> QualifiedName (ModuleName $ init xs) (last xs)

type_ :: Parser Type
type_ = namedType <|> try subType <|> tupleType

namedType :: Parser Type
namedType = NamedType <$> newID <*> name

subType :: Parser Type
subType = do
    id <- newID
    parameterTypes <- (openingParenthesis *> type_ `sepEndBy` comma <* closingParenthesis)
    fatArrow
    returnType <- type_
    return $ SubType id parameterTypes returnType

tupleType :: Parser Type
tupleType = TupleType <$> newID <*> (openingParenthesis *> type_ `sepEndBy` comma <* closingParenthesis)

parameter :: Parser Parameter
parameter = do
    name <- identifier
    colon
    paramType <- type_
    return $ Parameter name paramType

parameterList :: Parser ParameterList
parameterList = openingParenthesis *> parameter `sepEndBy` comma <* closingParenthesis

expr :: Parser Expr
expr = callExpr

callExpr :: Parser Expr
callExpr = do
    callee <- primaryExpr
    argumentLists <- many argumentList
    foldM (\a b -> (\id -> CallExpr id a b) <$> newID) callee argumentLists
    where
        argumentList :: Parser [Expr]
        argumentList = openingParenthesis *> expr `sepEndBy` comma <* closingParenthesis

primaryExpr :: Parser Expr
primaryExpr = nameExpr <|> stringLiteralExpr <|> blockExpr

nameExpr :: Parser Expr
nameExpr = NameExpr <$> newID <*> name

stringLiteralExpr :: Parser Expr
stringLiteralExpr = StringLiteralExpr <$> newID <*> stringLiteral

blockExpr :: Parser Expr
blockExpr = BlockExpr <$> newID <*> (openingBrace *> many stmt <* closingBrace)

stmt :: Parser Stmt
stmt = (ExprStmt <$> expr) <|> (DeclStmt <$> decl)

decl :: Parser Decl
decl = aliasDecl <|> importDecl <|> try structDecl <|> subDecl <|> foreignSubDecl

aliasDecl :: Parser Decl
aliasDecl = do
    id <- newID
    aliasKeyword
    name <- identifier
    equalsSign
    original <- type_
    return $ AliasDecl id name original

importDecl :: Parser Decl
importDecl = do
    id <- newID
    moduleName <- ModuleName <$> (importKeyword *> identifier `sepBy1` dot)
    return $ ImportDecl id moduleName

structDecl :: Parser Decl
structDecl = do
    id <- newID
    structKeyword
    name <- identifier
    openingBrace
    fields <- many field
    closingBrace
    return $ StructDecl id name fields
    where
        field :: Parser Field
        field = Field <$> identifier <*> (colon *> type_)

subDecl :: Parser Decl
subDecl = do
    id <- newID
    subKeyword
    name <- identifier
    params <- parameterList
    colon
    retType <- type_
    body <- blockExpr
    return $ SubDecl id name params retType body

foreignSubDecl :: Parser Decl
foreignSubDecl = do
    id <- newID
    foreignKeyword
    library <- ForeignLibrary <$> stringLiteral
    subKeyword
    cconv <- CallingConvention <$> name
    name <- identifier
    params <- parameterList
    colon
    retType <- type_
    return $ ForeignSubDecl id library cconv name params retType
