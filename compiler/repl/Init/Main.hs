{-# LANGUAGE OverloadedStrings #-}

module Init.Main
  ( init,
  )
where

import Control.Monad.Except
import Control.Monad.Reader
import qualified Data.Text as T
import Language.Mimsa.Project.Stdlib
import Language.Mimsa.Store.Storage
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Project
import Language.Mimsa.Types.Store.RootPath
import ReplNew.Persistence
import ReplNew.ReplM
import ReplNew.Types
import System.Directory
import Prelude hiding (init)

createReplConfig :: (MonadIO m) => Bool -> m ReplConfig
createReplConfig showLogs' = do
  path <- liftIO getCurrentDirectory
  pure $ ReplConfig (RootPath path) showLogs'

-- start a new project, using the stdlib bindings as a starting point
initialiseProject :: ReplM (Error Annotation) (Project Annotation)
initialiseProject = do
  rootPath <- asks rcRootPath
  saveAllInStore rootPath (prjStore stdlib)
  saveModulesInStore rootPath (prjModuleStore stdlib)
  _ <- mapError StoreErr (saveProject stdlib)
  replOutput ("New project created in " <> T.pack (show rootPath))
  pure stdlib

init :: Bool -> IO ()
init showLogs' = do
  cfg <- createReplConfig showLogs'
  _ <- runReplM cfg initialiseProject
  pure ()
