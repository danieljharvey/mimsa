{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Project.Stdlib (buildStdlib, stdlib) where

import Data.Functor
import Data.Text (Text)
import qualified Language.Mimsa.Actions.BindExpression as Actions
import qualified Language.Mimsa.Actions.BindType as Actions
import qualified Language.Mimsa.Actions.Monad as Actions
import Language.Mimsa.Parser
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Project

buildStdlib :: Either (Error Annotation) (Project Annotation)
buildStdlib =
  Actions.run mempty action >>= \(proj, _, _) -> pure proj
  where
    action = do
      addType "type Void"
      addType "type Maybe a = Just a | Nothing"
      addType "type Either e a = Left e | Right a"
      addType "type Task r a = Task ((a -> r) -> r)"
      addType "type Parser a  = Parser (String -> Maybe (a, String))"
      addType "type Unit = Unit"
      addType "type Redis r = Redis { read: (String -> Task r String), write: (String -> String -> Task r String) }"
      addBinding "id" "\\a -> a"
      addBinding "compose" "\\f -> \\g -> \\a -> f(g(a))"
      addBinding "not" "\\a -> if a then False else True"
      addBinding "runParser" "\\parser -> \\str -> match parser with (Parser p) -> match p(str) with  (Just (\"\", a)) -> (Just a) | _ -> (Nothing)"
      addBinding "fmapParser" "\\f -> \\parser -> Parser (\\str -> match parser with (Parser p) -> (let outcome = p(str); match outcome with (Just (a, rest)) -> (Just ((f(a),rest))) | _ -> (Nothing)))"
      addBinding "bindParser" "\\f -> \\parser -> Parser (\\str -> match parser with (Parser p) -> match p(str) with (Just (a, rest)) -> (let nextParser = f(a); match nextParser with (Parser b) -> b(rest)) | _ -> (Nothing))"
      addBinding "charParser" "Parser (\\s -> match s with ch ++ rest -> (Just ((rest, ch))) | _ -> (Nothing))"
      addBinding "predParser" "\\pred -> \\p -> Parser (\\s -> match parser.unwrap(p)(s) with (Just (rest, a)) -> (if pred(a) then (Just ((rest, a))) else (Nothing)) | _ -> (Nothing))"
      addBinding "pureTask" "\\a -> Task \\r -> r(a)"
      addBinding "arrayReduce" "let arrayReduce = \\f -> \\def -> \\as -> match as with [] -> def | [a, ...rest] -> (let val = f(a)(def); arrayReduce(f)(val)(rest)); arrayReduce"
      addBinding "arrayReverse" "arrayReduce((\\all -> \\a -> [ all ] <> a))([])"
      addBinding "arrayMap" "\\f -> arrayReduce((\\a -> \\all -> all <> [ f(a) ]))([])"
      addBinding "arrayFilter" "\\pred -> arrayReduce((\\a -> \\all -> if pred(a) then all <> [ a ] else all))([])"
      addBinding "array" "{ reduce: arrayReduce, reverse: arrayReverse, map: arrayMap, filter: arrayFilter }"
      addBinding "stringReduce" "let stringReduce = \\f -> \\def -> \\str -> match str with \"\" -> def | head ++ tail -> (let nextVal = f(def)(head); stringReduce(f)(nextVal)(tail)); stringReduce"
      addBinding "stringMap" "\\f -> stringReduce((\\total -> \\a -> total ++ f(a)))(\"\")"
      addBinding "stringFilter" "\\pred -> stringReduce((\\all -> \\a -> if pred(a) then all ++ a else all))(\"\")"
      addBinding "stringSplit" "\\char -> \\str -> arrayReverse(stringReduce((\\as -> \\a -> if (a == char) then [ \"\" ] <> as else match as with [] -> [] | [current, ...rest] -> [ current ++ a ] <> rest))([\"\"])(str))"
      addBinding "string" "{ reduce: stringReduce, map: stringMap, filter: stringFilter, split: stringSplit }"

addType :: Text -> Actions.ActionM ()
addType t =
  let dt = fromRight $ parseAndFormat typeDeclParser t
   in Actions.bindType t dt $> ()

addBinding :: Name -> Text -> Actions.ActionM ()
addBinding name b =
  let expr =
        fromRight $ parseAndFormat expressionParser b
   in Actions.bindExpression
        expr
        name
        b
        $> ()

fromRight :: (Show e) => Either e a -> a
fromRight = \case
  Left e -> error (show e)
  Right a -> a

stdlib :: Project Annotation
stdlib = fromRight buildStdlib
