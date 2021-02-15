{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Repl.Actions.UnitTests
  ( doAddUnitTest,
    doListTests,
  )
where

import Data.Foldable
import Data.Text (Text)
import Language.Mimsa.Actions.AddUnitTest
import Language.Mimsa.Printer
import Language.Mimsa.Project.Helpers
import Language.Mimsa.Project.UnitTest
import Language.Mimsa.Repl.Helpers
import Language.Mimsa.Repl.Types
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Project

doAddUnitTest ::
  Project Annotation ->
  Text ->
  TestName ->
  Expr Name Annotation ->
  ReplM Annotation (Project Annotation)
doAddUnitTest project input testName expr = do
  (newProject, unitTest) <-
    toReplM project (addUnitTest expr testName input)
  replPrint (prettyPrint unitTest)
  pure newProject

doListTests ::
  Project Annotation -> Maybe Name -> ReplM Annotation ()
doListTests project maybeName = do
  let fetchTestsForName =
        \name -> case lookupBindingName project name of
          Just exprHash -> getTestsForExprHash project exprHash
          Nothing -> mempty
  let tests = case maybeName of
        Just name -> fetchTestsForName name
        Nothing -> prjUnitTests project
  traverse_ (replPrint . prettyPrint) tests