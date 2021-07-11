{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Language.Mimsa.Backend.Runtimes
  ( Runtime (..),
    RuntimeName (..),
    exportRuntime,
    consoleRuntime,
    runtimeIsValid,
    outputIndexFile,
    indexFilename,
    getValidRuntimes,
    runtimes,
  )
where

import qualified Data.Aeson as JSON
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LB
import Data.Coerce (coerce)
import Data.Either (isRight)
import Data.FileEmbed
import Data.Map (Map)
import qualified Data.Map as M
import Data.Swagger
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Language.Mimsa.Backend.Javascript
import Language.Mimsa.Backend.Shared
import Language.Mimsa.Backend.Types
import Language.Mimsa.Parser
import Language.Mimsa.Printer
import Language.Mimsa.Typechecker.TcMonad
import Language.Mimsa.Typechecker.Unify
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Store
import Language.Mimsa.Types.Typechecker

-- these are saved in a file that is included in compilation
replCode :: LBS.ByteString
replCode =
  LBS.fromStrict $(embedFile "static/runtimes/commonjs/repl.js")

-- these are saved in a file that is included in compilation
taskServerCode :: LBS.ByteString
taskServerCode =
  LBS.fromStrict $(embedFile "static/runtimes/commonjs/task-server.js")

-- these are saved in a file that is included in compilation
redisTaskServerCode :: LBS.ByteString
redisTaskServerCode =
  LBS.fromStrict $(embedFile "static/runtimes/commonjs/redis-task-server.js")

newtype RuntimeName
  = RuntimeName Text
  deriving newtype
    ( Eq,
      Ord,
      Show,
      JSON.ToJSONKey,
      JSON.FromJSON,
      ToSchema
    )

data Runtime code = Runtime
  { rtName :: RuntimeName,
    rtDescription :: Text,
    rtMonoType :: MonoType,
    rtBackend :: Backend,
    rtCode :: code
  }

mtVar :: Text -> MonoType
mtVar t = MTVar mempty (TVName (TyVar t))

mtString :: MonoType
mtString = MTPrim mempty MTString

exportRuntime :: Runtime Javascript
exportRuntime =
  Runtime
    { rtName = RuntimeName "export",
      rtDescription = "Exports the expression",
      rtMonoType = mtVar "any",
      rtBackend = CommonJS,
      rtCode = "module.exports = { main };"
    }

consoleRuntime :: Runtime Javascript
consoleRuntime =
  Runtime
    { rtName = RuntimeName "console",
      rtDescription = "Logs a string expression to console",
      rtMonoType = mtString,
      rtBackend = CommonJS,
      rtCode = "console.log(main);"
    }

replRuntime :: Runtime Javascript
replRuntime =
  Runtime
    { rtName = RuntimeName "repl",
      rtDescription = "Runs a stateful repl session",
      rtMonoType =
        MTRecord
          mempty
          ( M.fromList
              [ ("prompt", mtString),
                ("intro", mtString),
                ("init", mtVar "A"),
                ( "next",
                  MTFunction
                    mempty
                    (mtVar "A")
                    ( MTFunction
                        mempty
                        mtString
                        ( MTPair
                            mempty
                            (mtVar "A")
                            mtString
                        )
                    )
                )
              ]
          ),
      rtBackend = CommonJS,
      rtCode = Javascript replCode
    }

fromRight :: (Printer e) => Either e a -> a
fromRight either' = case either' of
  Left e -> error (T.unpack $ prettyPrint e)
  Right a -> a

taskServerRuntime :: Runtime Javascript
taskServerRuntime =
  Runtime
    { rtName = RuntimeName "task-server",
      rtDescription = "Runs an asynchronous Task Server",
      rtMonoType =
        fromRight (parseAndFormat monoTypeParser "String -> Task r { data: String, status: Int }"),
      rtBackend = CommonJS,
      rtCode = Javascript taskServerCode
    }

redisTaskServerRuntime :: Runtime Javascript
redisTaskServerRuntime =
  Runtime
    { rtName = RuntimeName "redis-task-server",
      rtDescription = "Runs an asynchronous Task Server with Redis",
      rtMonoType =
        fromRight (parseAndFormat monoTypeParser "(Redis r) -> String -> Task r { data: String, status: Int }"),
      rtBackend = CommonJS,
      rtCode = Javascript redisTaskServerCode
    }

runtimeIsValid :: Runtime a -> MonoType -> Either TypeError ()
runtimeIsValid runtime mt =
  runTcMonad
    mempty
    $ unify (rtMonoType runtime) mt >> pure ()

--------

runtimes :: Map RuntimeName (Runtime Javascript)
runtimes =
  M.fromList $
    foldr
      ( \rt as ->
          as <> [(rtName rt, rt)]
      )
      mempty
      [ consoleRuntime,
        exportRuntime,
        replRuntime,
        taskServerRuntime,
        redisTaskServerRuntime
      ]

getValidRuntimes :: MonoType -> Map RuntimeName (Runtime Javascript)
getValidRuntimes mt =
  M.filter (\rt -> isRight $ runtimeIsValid rt mt) runtimes

--------

bsFromText :: Text -> LBS.ByteString
bsFromText = LB.fromChunks . return . T.encodeUtf8

outputIndexFile :: Runtime Javascript -> ExprHash -> Javascript
outputIndexFile runtime exprHash =
  Javascript ("const main = require('./" <> moduleFilename CommonJS exprHash <> "').main;\n")
    <> rtCode runtime

indexFilename :: Runtime code -> ExprHash -> LBS.ByteString
indexFilename runtime hash' =
  case rtBackend runtime of
    CommonJS ->
      bsFromText
        ( "index-" <> coerce (rtName runtime) <> "-"
            <> prettyPrint hash'
            <> ".js"
        )
