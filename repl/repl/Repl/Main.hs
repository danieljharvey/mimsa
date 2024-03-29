{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Repl.Main
  ( repl,
  )
where

import Control.Monad.Except
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Logger
import Control.Monad.Trans.Class (lift)
import Data.Text (Text)
import qualified Data.Text as T
import Language.Mimsa.Core
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Project
import Language.Mimsa.Types.Store.RootPath
import Repl.Actions (doReplAction)
import Repl.Helpers
import Repl.Parser (replParser)
import Repl.Persistence
import Repl.ReplM
import Repl.Types
import qualified Shared.LoadProject as Shared
import System.Console.Haskeline
import System.Directory
import Text.Megaparsec

createReplConfig :: (MonadIO m) => Bool -> m ReplConfig
createReplConfig showLogs' = do
  path <- liftIO getCurrentDirectory
  pure $ ReplConfig (RootPath path) showLogs'

getProject :: ReplM (Error Annotation) (Project Annotation)
getProject =
  do
    maybeProject <- Shared.loadProject
    case maybeProject of
      Right prj -> do
        let moduleItems = length . prjModuleStore $ prj
        replOutput ("Successfully loaded project." :: Text)
        replOutput $ T.pack (show moduleItems) <> " modules found"
        pure prj
      Left e -> do
        logDebugN (prettyPrint e)
        replOutput @Text "Failed to load project, have you initialised a project in this folder?"
        throwError e

repl :: Bool -> IO ()
repl showLogs' = do
  cfg <- createReplConfig showLogs'
  _ <- runReplM cfg replLoop
  pure ()

replLoop :: ReplM (Error Annotation) ()
replLoop = do
  env <- getProject
  _ <- doReplAction env Help
  runInputT defaultSettings (loop env)
  where
    loop ::
      Project Annotation ->
      InputT (ReplM (Error Annotation)) ()
    loop exprs' = do
      minput <- getInputLine ":> "
      case minput of
        Nothing -> return ()
        Just ":quit" -> return ()
        Just input -> do
          newEnv <- lift $ parseCommand exprs' (T.pack input)
          loop newEnv

parseCommand ::
  Project Annotation ->
  Text ->
  ReplM (Error Annotation) (Project Annotation)
parseCommand env input =
  case parse replParser "<repl>" input of
    Left errBundle -> do
      outputErrorAsDiagnostic (ParseError input errBundle)
      pure env
    Right replAction -> do
      newExprs <- doReplAction env replAction
      _ <- mapReplError StoreErr (saveProject newExprs)
      pure newExprs
