{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Language.Mimsa.Core.Types.AST.Expr
  ( Expr (..),
  )
where

import qualified Data.Aeson as JSON
import Data.Bifunctor (first)
import Data.Bifunctor.TH
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NE
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import GHC.Generics (Generic)
import GHC.Natural
import Language.Mimsa.Core.Printer
import Language.Mimsa.Core.Types.AST.Identifier
import Language.Mimsa.Core.Types.AST.Literal (Literal)
import Language.Mimsa.Core.Types.AST.Operator
import Language.Mimsa.Core.Types.AST.Pattern
import Language.Mimsa.Core.Types.Identifiers
import Language.Mimsa.Core.Types.Module.ModuleName
import Language.Mimsa.Core.Types.Type.MonoType
import Language.Mimsa.Core.Utils
import Prettyprinter

-------

-- |
-- The main expression type that we parse from syntax
-- `var` is the type of variables. When we parse them they are
-- string-based `Name`, but after substitution they become a `Variable`
-- which is either a string or a numbered variable
data Expr var ann
  = -- | a literal, such as String, Int, Boolean
    MyLiteral
      { expAnn :: ann,
        expLit :: Literal
      }
  | MyAnnotation
      { expAnn :: ann,
        expType :: Type ann,
        expExpr :: Expr var ann
      }
  | -- | a named variable
    MyVar
      { expAnn :: ann,
        expModuleName :: Maybe ModuleName,
        expVar :: var
      }
  | -- | binder, expr, body
    MyLet
      { expAnn :: ann,
        expBinder :: Identifier var ann,
        expExpr :: Expr var ann,
        expBody :: Expr var ann
      }
  | -- | pat, expr, body
    MyLetPattern
      { expAnn :: ann,
        expPattern :: Pattern var ann,
        expExpr :: Expr var ann,
        expBody :: Expr var ann
      }
  | -- | a `f` b
    MyInfix
      { expAnn :: ann,
        expOperator :: Operator,
        expExpr :: Expr var ann,
        expBody :: Expr var ann
      }
  | -- | binder, body
    MyLambda
      { expAnn :: ann,
        expBinder :: Identifier var ann,
        expBody :: Expr var ann
      }
  | -- | function, argument
    MyApp
      { expAnn :: ann,
        expFunc :: Expr var ann,
        expArg :: Expr var ann
      }
  | -- | expr, thencase, elsecase
    MyIf
      { expAnn :: ann,
        expPred :: Expr var ann,
        expThen :: Expr var ann,
        expElse :: Expr var ann
      }
  | -- | (a,b,...)
    MyTuple
      { expAnn :: ann,
        expA :: Expr var ann,
        expB :: NE.NonEmpty (Expr var ann)
      }
  | -- | (a,b,c).1 == a
    MyTupleAccess
      { expAnn :: ann,
        expTuple :: Expr var ann,
        expIndex :: Natural
      }
  | -- | { dog: MyLiteral (MyInt 1), cat: MyLiteral (MyInt 2) }
    MyRecord
      { expAnn :: ann,
        expRecordItems :: Map Name (Expr var ann)
      }
  | -- | a.foo
    MyRecordAccess
      { expAnn :: ann,
        expRecord :: Expr var ann,
        expKey :: Name
      }
  | MyArray
      { expAnn :: ann,
        expArrayItems :: [Expr var ann]
      }
  | -- | use a constructor by name
    MyConstructor
      { expAnn :: ann,
        expModuleName :: Maybe ModuleName,
        expTyCon :: TyCon
      }
  | -- | expr, [(pattern, expr)]
    MyPatternMatch
      { expAnn :: ann,
        expExpr :: Expr var ann,
        expPatterns :: [(Pattern var ann, Expr var ann)]
      }
  | -- | name
    MyTypedHole {expAnn :: ann, expTypedHoleName :: var}
  deriving stock (Eq, Ord, Show, Functor, Foldable, Generic)
  deriving anyclass (JSON.FromJSON, JSON.ToJSON)

$(deriveBifunctor ''Expr)

data InfixBit var ann
  = IfStart (Expr var ann)
  | IfMore Operator (Expr var ann)
  deriving stock (Show)

getInfixList :: Expr Name ann -> NE.NonEmpty (InfixBit Name ann)
getInfixList expr = case expr of
  (MyInfix _ op a b) ->
    let start = getInfixList a
     in start <> NE.fromList [IfMore op b]
  other -> NE.fromList [IfStart other]

prettyInfixList :: NE.NonEmpty (InfixBit Name ann) -> Doc style
prettyInfixList (ifHead :| ifRest) =
  let printInfixBit (IfMore op expr') = prettyDoc op <+> printSubExpr expr'
      printInfixBit (IfStart expr') = printSubExpr expr'
   in printInfixBit ifHead <+> align (vsep (printInfixBit <$> ifRest))

-- when on multilines, indent by `i`, if not then nothing
indentMulti :: Int -> Doc style -> Doc style
indentMulti i doc = flatAlt (indent i doc) doc

prettyLet ::
  Identifier Name ann ->
  Expr Name ann ->
  Expr Name ann ->
  Doc style
prettyLet var expr1 expr2 =
  let (args, letExpr, maybeMt) = splitExpr expr1
      prettyVar = case maybeMt of
        Just mt ->
          "(" <> prettyDoc var <> ":" <+> prettyDoc mt <> ")"
        Nothing ->
          prettyDoc var
   in group
        ( "let"
            <+> prettyVar
              <> prettyArgs args
            <+> "="
              <> line
              <> indentMulti 2 (prettyDoc letExpr)
              <> newlineOrIn
              <> prettyDoc expr2
        )
  where
    prettyArgs [] = ""
    prettyArgs as = space <> hsep (prettyDoc <$> as)

    splitExpr expr =
      case expr of
        (MyLambda _ a rest) ->
          let (as, expr', mt) = splitExpr rest
           in ([a] <> as, expr', mt)
        (MyAnnotation _ mt annExpr) ->
          let (as, expr', _) = splitExpr annExpr
           in (as, expr', Just mt)
        other -> ([], other, Nothing)

prettyLetPattern ::
  Pattern Name ann ->
  Expr Name ann ->
  Expr Name ann ->
  Doc style
prettyLetPattern pat expr body =
  group
    ( "let"
        <+> printSubPattern pat
        <+> "="
          <> line
          <> indentMulti 2 (printSubExpr expr)
          <> newlineOrIn
          <> printSubExpr body
    )

newlineOrIn :: Doc style
newlineOrIn = flatAlt (";" <> line <> line) " in "

prettyTuple :: Expr Name ann -> NE.NonEmpty (Expr Name ann) -> Doc style
prettyTuple a as =
  group
    ( "("
        <> align
          ( vsep
              ( punctuate
                  ","
                  (printSubExpr <$> ([a] <> NE.toList as))
              )
          )
        <> ")"
    )

prettyLambda ::
  Identifier Name ann ->
  Expr Name ann ->
  Doc style
prettyLambda binder expr =
  group
    ( vsep
        [ "\\"
            <> prettyDoc binder
            <+> "->",
          indentMulti 2 $
            prettyDoc expr
        ]
    )

prettyRecord ::
  Map Name (Expr Name ann) ->
  Doc style
prettyRecord map' =
  let items = M.toList map'
      printRow i (name, val) =
        let item = case val of
              (MyVar _ _ vName)
                | vName == name ->
                    prettyDoc name
              _ ->
                prettyDoc name
                  <> ":"
                  <+> printSubExpr val
         in item <> if i < length items then "," else ""
   in case items of
        [] -> "{}"
        rows ->
          let prettyRows = mapWithIndex printRow rows
           in group
                ( "{"
                    <+> align
                      ( vsep
                          prettyRows
                      )
                    <+> "}"
                )

prettyArray :: [Expr Name ann] -> Doc style
prettyArray items =
  let printRow i val =
        printSubExpr val
          <> if i < length items then "," else ""
   in case items of
        [] -> "[]"
        rows ->
          let prettyRows = mapWithIndex printRow rows
           in group
                ( "["
                    <+> align
                      ( vsep
                          prettyRows
                      )
                    <+> "]"
                )

prettyIf ::
  Expr Name ann ->
  Expr Name ann ->
  Expr Name ann ->
  Doc style
prettyIf if' then' else' =
  group
    ( vsep
        [ "if"
            <+> wrapInfix if',
          "then",
          indentMulti 2 (printSubExpr then'),
          "else",
          indentMulti 2 (printSubExpr else')
        ]
    )

prettyPatternMatch ::
  Expr Name ann ->
  [(Pattern Name ann, Expr Name ann)] ->
  Doc style
prettyPatternMatch sumExpr matches =
  "match"
    <+> printSubExpr sumExpr
    <+> "with"
    <+> line
      <> indent
        2
        ( align $
            vsep
              ( zipWith
                  (<+>)
                  (" " : repeat "|")
                  (printMatch <$> matches)
              )
        )
  where
    printMatch (construct, expr') =
      printSubPattern construct
        <+> "->"
        <+> line
          <> indentMulti 4 (printSubExpr expr')

-- just for debugging
instance (Printer var) => Printer (Expr (var, a) ann) where
  prettyDoc = prettyDoc . first (mkName . prettyPrint . fst)

instance Printer (Expr Name ann) where
  prettyDoc (MyLiteral _ l) =
    prettyDoc l
  prettyDoc (MyAnnotation _ mt expr) =
    "(" <> prettyDoc expr <+> ":" <+> prettyDoc mt <> ")"
  prettyDoc (MyVar _ (Just modName) var) =
    prettyDoc modName <> "." <> prettyDoc var
  prettyDoc (MyVar _ Nothing var) =
    prettyDoc var
  prettyDoc (MyLet _ var expr1 expr2) =
    prettyLet var expr1 expr2
  prettyDoc (MyLetPattern _ pat expr body) =
    prettyLetPattern pat expr body
  prettyDoc wholeExpr@MyInfix {} =
    group (prettyInfixList (getInfixList wholeExpr))
  prettyDoc (MyLambda _ binder expr) =
    prettyLambda binder expr
  prettyDoc (MyApp _ func arg) =
    prettyDoc func <+> wrapInfix arg
  prettyDoc (MyRecordAccess _ expr name) =
    prettyDoc expr <> "." <> prettyDoc name
  prettyDoc (MyTupleAccess _ expr index) =
    prettyDoc expr <> "." <> prettyDoc index
  prettyDoc (MyIf _ if' then' else') =
    prettyIf if' then' else'
  prettyDoc (MyTuple _ a as) =
    prettyTuple a as
  prettyDoc (MyRecord _ map') =
    prettyRecord map'
  prettyDoc (MyArray _ items) = prettyArray items
  prettyDoc (MyConstructor _ (Just modName) name) =
    prettyDoc modName <> "." <> prettyDoc name
  prettyDoc (MyConstructor _ Nothing name) =
    prettyDoc name
  prettyDoc (MyTypedHole _ name) = "?" <> prettyDoc name
  prettyDoc (MyPatternMatch _ expr matches) =
    prettyPatternMatch expr matches

wrapInfix :: Expr Name ann -> Doc style
wrapInfix val = case val of
  val'@MyInfix {} -> inParens val'
  other -> printSubExpr other

inParens :: Expr Name ann -> Doc style
inParens = parens . prettyDoc

-- print simple things with no brackets, and complex things inside brackets
printSubExpr :: Expr Name ann -> Doc style
printSubExpr expr = case expr of
  all'@MyLet {} -> inParens all'
  all'@MyLambda {} -> inParens all'
  all'@MyIf {} -> inParens all'
  all'@MyApp {} -> inParens all'
  all'@MyTuple {} -> inParens all'
  all'@MyPatternMatch {} -> inParens all'
  a -> prettyDoc a
