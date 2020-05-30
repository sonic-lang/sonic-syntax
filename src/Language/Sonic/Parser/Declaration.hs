{-# LANGUAGE NamedFieldPuns #-}

module Language.Sonic.Parser.Declaration
  ( declParser
  , simpleDeclParser
  , dataDeclParser
  , classDeclParser
  , instanceDeclParser
  , signatureDeclParser
  , valueDeclParser
  , functionDeclParser
  , functionClauseParser
  , whereClauseParser
  )
where
import           Control.Applicative            ( Alternative(..) )
import           Control.Applicative.Combinators
                                                ( sepBy1
                                                , sepEndBy
                                                , optional
                                                )

import           Text.Megaparsec                ( try
                                                , (<?>)
                                                )

import           Language.Sonic.Parser.Internal.Source
                                                ( Source )
import           Language.Sonic.Parser.Internal.Location
                                                ( Offset
                                                , withOffset
                                                )
import           Language.Sonic.Parser.Internal.Parse
                                                ( Parse )
import           Language.Sonic.Parser.Internal.Lexer
                                                ( symbol
                                                , word
                                                )
import           Language.Sonic.Parser.Name     ( varNameParser
                                                , ctorNameParser
                                                , tyCtorNameParser
                                                , classNameParser
                                                )
import           Language.Sonic.Parser.Path     ( pathParser )
import           Language.Sonic.Parser.Type     ( typeParser
                                                , contextParser
                                                , tyVarBinderParser
                                                )
import           Language.Sonic.Parser.Pattern  ( patParser )
import           Language.Sonic.Parser.Expression
                                                ( exprParser
                                                , guardParser
                                                )
import           Language.Sonic.Syntax.Sequence ( Sequence(..) )
import           Language.Sonic.Syntax.Declaration
                                                ( Decl(..)
                                                , SimpleDecl(..)
                                                , SignatureDecl(..)
                                                , ValueDecl(..)
                                                , FunctionDecl(..)
                                                , FunctionClause(..)
                                                , DataDecl(..)
                                                , ClassDecl(..)
                                                , InstanceDecl(..)
                                                , WhereClause(..)
                                                )

instanceDeclParser :: Source s => Parse s (InstanceDecl Offset)
instanceDeclParser = do
  word "instance"
  context   <- optional . try $ withOffset contextParser <* symbol "=>"
  className <- withOffset $ pathParser classNameParser
  types     <- Sequence <$> many (withOffset typeParser)
  methods   <- optional . withOffset $ whereClauseParser simpleDeclParser
  pure InstanceDecl { context, className, types, methods }

classDeclParser :: Source s => Parse s (ClassDecl Offset)
classDeclParser = do
  word "class"
  context   <- optional . try $ withOffset contextParser <* symbol "<="
  className <- withOffset classNameParser
  vars      <- Sequence <$> many (withOffset tyVarBinderParser)
  methods   <- optional . withOffset $ whereClauseParser simpleDeclParser
  pure ClassDecl { context, className, vars, methods }

dataDeclParser :: Source s => Parse s (DataDecl Offset)
dataDeclParser = do
  word "data"
  dataName <- withOffset tyCtorNameParser
  vars     <- Sequence <$> many (withOffset tyVarBinderParser)
  ctors    <- optional . withOffset $ whereClauseParser
    (signatureDeclParser ctorNameParser)
  pure DataDecl { dataName, vars, ctors }

whereClauseParser
  :: Source s => Parse s (d Offset) -> Parse s (WhereClause d Offset)
whereClauseParser d = do
  word "where"
  decls <- withOffset (manyDecls <|> oneDecl)
  pure $ WhereClause decls
 where
  oneDecl = do
    decl <- withOffset d
    pure $ Sequence [decl]
  manyDecls = do
    symbol "{"
    ds <- withOffset d `sepEndBy` symbol ";"
    symbol "}"
    pure $ Sequence ds

functionClauseParser :: Source s => Parse s (FunctionClause Offset)
functionClauseParser = do
  pats  <- Sequence <$> some (withOffset patParser)
  guard <- optional $ withOffset guardParser
  symbol "="
  body     <- withOffset exprParser
  bindings <- optional . withOffset $ whereClauseParser simpleDeclParser
  pure FunctionClause { pats, guard, body, bindings }

functionDeclParser :: Source s => Parse s (FunctionDecl Offset)
functionDeclParser = do
  name    <- withOffset varNameParser
  clauses <- withOffset functionClausesParser
  pure FunctionDecl { name, clauses }
 where
  functionClausesParser = manyClauses <|> oneClause
  manyClauses           = do
    symbol "|"
    clauses <- withOffset functionClauseParser `sepBy1` symbol "|"
    pure $ Sequence clauses
  oneClause = do
    clause <- withOffset functionClauseParser
    pure $ Sequence [clause]


valueDeclParser :: Source s => Parse s (ValueDecl Offset)
valueDeclParser = do
  pat <- withOffset patParser
  symbol "="
  body     <- withOffset exprParser
  bindings <- optional . withOffset $ whereClauseParser simpleDeclParser
  pure ValueDecl { pat, body, bindings }

signatureDeclParser
  :: Source s => Parse s (n Offset) -> Parse s (SignatureDecl n Offset)
signatureDeclParser n = do
  name <- withOffset n
  symbol "::"
  type_ <- withOffset typeParser
  pure SignatureDecl { name, type_ }

simpleDeclParser :: Source s => Parse s (SimpleDecl Offset)
simpleDeclParser =
  try (Signature <$> signatureDeclParser varNameParser)
    <|> try (Value <$> valueDeclParser)
    <|> Function
    <$> functionDeclParser
    <?> "simple declaration"

declParser :: Source s => Parse s (Decl Offset)
declParser =
  Data
    <$> dataDeclParser
    <|> Class
    <$> classDeclParser
    <|> Instance
    <$> instanceDeclParser
    <|> Simple
    <$> simpleDeclParser
    <?> "declaration"
