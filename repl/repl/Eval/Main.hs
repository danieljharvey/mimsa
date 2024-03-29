module Eval.Main
  ( eval,
  )
where

import Control.Monad.Except
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Either
import Data.Text (Text)
import qualified Language.Mimsa.Actions.Helpers.Parse as Actions
import qualified Language.Mimsa.Actions.Modules.Evaluate as Actions
import Language.Mimsa.Core
import Language.Mimsa.Project.Stdlib
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Store.RootPath
import Repl.Helpers
import Repl.ReplM
import Repl.Types
import qualified Shared.LoadProject as Shared
import System.Directory
import System.Exit
import Prelude hiding (init)

createReplConfig :: (MonadIO m) => Bool -> m ReplConfig
createReplConfig showLogs' = do
  path <- liftIO getCurrentDirectory
  pure $ ReplConfig (RootPath path) showLogs'

---------

-- evaluate an expression
evalInput :: Text -> ReplM (Error Annotation) ExitCode
evalInput input = do
  maybeProject <- Shared.loadProject
  -- use project if we're in one, if not, stdlib
  let project = fromRight stdlib maybeProject
  let action = do
        expr <- Actions.parseExpr input
        Actions.evaluateModule expr mempty
  result <-
    (Right <$> toReplM project action)
      `catchError` (pure . Left)

  let returnCode =
        if isRight result
          then ExitSuccess
          else ExitFailure 1
  --
  pure returnCode

eval :: Bool -> Text -> IO ()
eval showLogs' input = do
  cfg <- createReplConfig showLogs'
  exitCode <- runReplM cfg (evalInput input)
  case exitCode of
    Right ec -> exitWith ec
    _ -> exitWith $ ExitFailure 1
