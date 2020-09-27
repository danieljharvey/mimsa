{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Parser.Language
  ( parseExpr,
    parseExpr',
    expressionParser,
    nameParser,
    tyConParser,
    typeDeclParser,
  )
where

import Control.Applicative ((<|>), optional)
import Control.Monad ((>=>))
import Data.Functor (($>))
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Language.Mimsa.Parser.Parser (Parser)
import qualified Language.Mimsa.Parser.Parser as P
import Language.Mimsa.Types
  ( DataType (DataType),
    Expr (..),
    Literal (MyBool, MyInt, MyString, MyUnit),
    Name,
    StringType (StringType),
    TyCon,
    TypeName (..),
    safeMkName,
    safeMkTyCon,
  )

type ParserExpr = Expr Name

-- parse expr, using it all up
parseExpr :: Text -> Either Text ParserExpr
parseExpr input = P.runParser (P.thenOptionalSpace expressionParser) input
  >>= \(leftover, a) ->
    if T.length leftover == 0
      then Right a
      else Left ("Leftover input: >>>" <> leftover <> "<<<")

parseExpr' :: Text -> Either Text ParserExpr
parseExpr' input = snd <$> P.runParser expressionParser input

failer :: Parser ParserExpr
failer = P.parseFail (\input -> "Could not parse expression for >>>" <> input <> "<<<")

expressionParser :: Parser ParserExpr
expressionParser =
  let parsers =
        literalParser
          <|> complexParser
          <|> varParser
          <|> constructorParser
   in orInBrackets parsers
        <|> failer

inBrackets :: Parser a -> Parser a
inBrackets = P.between2 '(' ')'

orInBrackets :: Parser a -> Parser a
orInBrackets parser = parser <|> inBrackets parser

literalParser :: Parser ParserExpr
literalParser =
  boolParser
    <|> unitParser
    <|> intParser
    <|> stringParser

complexParser :: Parser ParserExpr
complexParser =
  letPairParser
    <|> recordParser
    <|> letParser
    <|> ifParser
    <|> appParser
    <|> pairParser
    <|> recordAccessParser
    <|> lambdaParser
    <|> typeParser
    <|> constructorAppParser
    <|> caseMatchParser

protectedNames :: Set Text
protectedNames =
  S.fromList
    [ "let",
      "in",
      "if",
      "then",
      "else",
      "case",
      "of",
      "type",
      "otherwise",
      "True",
      "False",
      "Unit"
    ]

----

boolParser :: Parser ParserExpr
boolParser = trueParser <|> falseParser

trueParser :: Parser ParserExpr
trueParser = P.literal "True" $> MyLiteral (MyBool True)

falseParser :: Parser ParserExpr
falseParser = P.literal "False" $> MyLiteral (MyBool False)

-----

unitParser :: Parser ParserExpr
unitParser = P.literal "Unit" $> MyLiteral MyUnit

-----

intParser :: Parser ParserExpr
intParser = MyLiteral . MyInt <$> P.integer

-----

stringParser :: Parser ParserExpr
stringParser = MyLiteral . MyString . StringType <$> P.between '"'

-----

varParser :: Parser ParserExpr
varParser = MyVar <$> nameParser

---

nameParser :: Parser Name
nameParser =
  P.maybePred
    P.identifier
    (inProtected >=> safeMkName)

inProtected :: Text -> Maybe Text
inProtected tx = if S.member tx protectedNames then Nothing else Just tx

---

constructorParser :: Parser ParserExpr
constructorParser = MyConstructor <$> tyConParser

tyConParser :: Parser TyCon
tyConParser =
  P.maybePred
    P.identifier
    (inProtected >=> safeMkTyCon)

-----

letParser :: Parser ParserExpr
letParser = letInParser <|> letNewlineParser

letNameIn :: Parser Name
letNameIn = do
  _ <- P.thenSpace (P.literal "let")
  name <- P.thenSpace nameParser
  _ <- P.thenSpace (P.literal "=")
  pure name

letInParser :: Parser ParserExpr
letInParser = do
  name <- letNameIn
  expr <- P.thenSpace expressionParser
  _ <- P.thenSpace (P.literal "in")
  MyLet name expr <$> expressionParser

letNewlineParser :: Parser ParserExpr
letNewlineParser = do
  name <- letNameIn
  expr <- expressionParser
  _ <- literalWithSpace ";"
  MyLet name expr <$> expressionParser

-----

spacedName :: Parser Name
spacedName = do
  _ <- P.space0
  name <- nameParser
  _ <- P.space0
  pure name

-----

letPairParser :: Parser ParserExpr
letPairParser = MyLetPair <$> binder1 <*> binder2 <*> equalsParser <*> inParser
  where
    binder1 = do
      _ <- P.thenSpace (P.literal "let")
      _ <- P.literal "("
      spacedName
    binder2 = do
      _ <- P.literal ","
      name <- spacedName
      _ <- P.thenSpace (P.literal ")")
      pure name

-----

equalsParser :: Parser ParserExpr
equalsParser =
  P.right (P.thenSpace (P.literal "=")) (P.thenSpace expressionParser)

inParser :: Parser ParserExpr
inParser = P.right (P.thenSpace (P.literal "in")) expressionParser

-----

lambdaParser :: Parser ParserExpr
lambdaParser = MyLambda <$> slashNameBinder <*> arrowExprBinder

-- matches \varName
slashNameBinder :: Parser Name
slashNameBinder = do
  _ <- P.literal "\\"
  _ <- P.space0
  P.thenSpace nameParser

arrowExprBinder :: Parser ParserExpr
arrowExprBinder = P.right (P.thenSpace (P.literal "->")) expressionParser

-----

appFunc :: Parser ParserExpr
appFunc = recordAccessParser <|> inBrackets lambdaParser <|> varParser

appParser :: Parser ParserExpr
appParser = do
  app <- appFunc
  exprs <- P.oneOrMore (withOptionalSpace exprInBrackets)
  pure (foldl MyApp app exprs)

literalWithSpace :: Text -> Parser ()
literalWithSpace tx = () <$ withOptionalSpace (P.literal tx)

withOptionalSpace :: Parser a -> Parser a
withOptionalSpace p = do
  _ <- P.space0
  a <- p
  _ <- P.space0
  pure a

exprInBrackets :: Parser ParserExpr
exprInBrackets = do
  literalWithSpace "("
  expr <- expressionParser
  literalWithSpace ")"
  _ <- P.space0
  pure expr

-----

recordParser :: Parser ParserExpr
recordParser = fullRecordParser <|> emptyRecordParser

emptyRecordParser :: Parser ParserExpr
emptyRecordParser = do
  literalWithSpace "{"
  literalWithSpace "}"
  pure (MyRecord mempty)

fullRecordParser :: Parser ParserExpr
fullRecordParser = do
  literalWithSpace "{"
  args <- P.zeroOrMore (P.left recordItemParser (P.left (P.literal ",") P.space0))
  last' <- recordItemParser
  literalWithSpace "}"
  pure (MyRecord (M.fromList $ args <> [last']))

recordItemParser :: Parser (Name, ParserExpr)
recordItemParser = do
  name <- nameParser
  literalWithSpace ":"
  expr <- withOptionalSpace expressionParser
  pure (name, expr)

-----

recordAccessParser :: Parser ParserExpr
recordAccessParser =
  recordAccessParser3
    <|> recordAccessParser2
    <|> recordAccessParser1

recordAccessParser1 :: Parser ParserExpr
recordAccessParser1 = do
  expr <- varParser
  name <- dotName
  _ <- P.space0
  pure (MyRecordAccess expr name)

dotName :: Parser Name
dotName = do
  _ <- P.literal "."
  nameParser

recordAccessParser2 :: Parser ParserExpr
recordAccessParser2 = do
  expr <- varParser
  name <- dotName
  name2 <- dotName
  _ <- P.space0
  pure (MyRecordAccess (MyRecordAccess expr name) name2)

recordAccessParser3 :: Parser ParserExpr
recordAccessParser3 = do
  expr <- varParser
  _ <- P.literal "."
  name <- nameParser
  _ <- P.literal "."
  name2 <- nameParser
  _ <- P.literal "."
  name3 <- nameParser
  _ <- P.space0
  pure (MyRecordAccess (MyRecordAccess (MyRecordAccess expr name) name2) name3)

-----

ifParser :: Parser ParserExpr
ifParser = MyIf <$> predParser <*> thenParser <*> elseParser

predParser :: Parser ParserExpr
predParser = P.right (P.thenSpace (P.literal "if")) expressionParser

thenParser :: Parser ParserExpr
thenParser = P.right (P.thenSpace (P.literal "then")) expressionParser

elseParser :: Parser ParserExpr
elseParser = P.right (P.thenSpace (P.literal "else")) expressionParser

-----

pairParser :: Parser ParserExpr
pairParser = do
  _ <- P.literal "("
  _ <- P.space0
  exprA <- expressionParser
  _ <- P.space0
  _ <- P.literal ","
  _ <- P.space0
  exprB <- expressionParser
  _ <- P.space0
  _ <- P.literal ")"
  _ <- P.space0
  pure $ MyPair exprA exprB

-----

typeDeclParser :: Parser DataType
typeDeclParser = typeDeclParserWithCons <|> typeDeclParserEmpty

typeDeclParserEmpty :: Parser DataType
typeDeclParserEmpty = do
  _ <- P.thenSpace (P.literal "type")
  tyName <- tyConParser
  pure (DataType tyName mempty mempty)

typeDeclParserWithCons :: Parser DataType
typeDeclParserWithCons = do
  _ <- P.thenSpace (P.literal "type")
  tyName <- P.thenSpace tyConParser
  tyArgs <- P.zeroOrMore (P.left nameParser P.space1)
  _ <- P.thenSpace (P.literal "=")
  constructors <- manyTypeConstructors <|> oneTypeConstructor
  pure $ DataType tyName tyArgs constructors

--------

typeParser :: Parser ParserExpr
typeParser =
  MyData <$> (typeDeclParserWithCons <|> typeDeclParserEmpty)
    <*> (inExpr <|> inNewLineExpr)

inNewLineExpr :: Parser ParserExpr
inNewLineExpr = do
  _ <- literalWithSpace ";"
  expressionParser

inExpr :: Parser ParserExpr
inExpr = do
  _ <- P.space0
  _ <- P.thenSpace (P.literal "in")
  expressionParser

----

manyTypeConstructors :: Parser (Map TyCon [TypeName])
manyTypeConstructors = do
  cons <- NE.toList <$> P.oneOrMore (P.left oneTypeConstructor (P.thenSpace (P.literal "|")))
  lastCons <- oneTypeConstructor
  pure (mconcat cons <> lastCons)

oneTypeConstructor :: Parser (Map TyCon [TypeName])
oneTypeConstructor = do
  name <- tyConParser
  args <- P.zeroOrMore (P.right P.space1 typeNameParser)
  pure (M.singleton name args)

-----

typeNameParser :: Parser TypeName
typeNameParser =
  emptyConsParser <|> varNameParser <|> inBrackets parameterisedConsParser <|> varNameParser

emptyConsParser :: Parser TypeName
emptyConsParser = ConsName <$> tyConParser <*> pure mempty

parameterisedConsParser :: Parser TypeName
parameterisedConsParser = do
  c <- tyConParser
  params <- P.zeroOrMore (P.right P.space1 typeNameParser)
  pure $ ConsName c params

varNameParser :: Parser TypeName
varNameParser = VarName <$> nameParser

---
--
constructorAppParser :: Parser ParserExpr
constructorAppParser = do
  cons <- tyConParser
  exprs <- P.oneOrMore (P.right P.space1 (orInBrackets expressionParser))
  pure (foldl MyConsApp (MyConstructor cons) exprs)

----------
caseExprOfParser :: Parser ParserExpr
caseExprOfParser = do
  _ <- P.thenSpace (P.literal "case")
  sumExpr <- expressionParser
  _ <- P.thenSpace (P.literal "of")
  pure sumExpr

caseMatchParser :: Parser ParserExpr
caseMatchParser = do
  sumExpr <- caseExprOfParser
  matches <-
    matchesParser <|> pure <$> matchParser
  catchAll <-
    optional (otherwiseParser (not . null $ matches))
  pure $ MyCaseMatch sumExpr matches catchAll

otherwiseParser :: Bool -> Parser ParserExpr
otherwiseParser needsBar = do
  if needsBar
    then () <$ P.thenSpace (P.literal "|")
    else pure ()
  _ <- P.thenSpace (P.literal "otherwise")
  expressionParser

matchesParser :: Parser (NonEmpty (TyCon, ParserExpr))
matchesParser = do
  cons <- P.zeroOrMore (P.left matchParser (P.thenSpace (P.literal "|")))
  lastCons <- matchParser
  pure $ NE.fromList (cons <> [lastCons])

matchParser :: Parser (TyCon, Expr Name)
matchParser = (,) <$> P.thenSpace tyConParser <*> expressionParser