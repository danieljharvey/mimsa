{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Language.Mimsa.Backend.Shared
  ( symlinkedOutputPath,
    zipFileOutputPath,
    indexOutputFilename,
    storeExprFilename,
    fileExtension,
    stdlibFilename,
    outputStdlib,
  )
where

import Data.FileEmbed
import Data.Text (Text)
import qualified Data.Text.Encoding as T
import Language.Mimsa.Backend.Types
import Language.Mimsa.Core

-- these are saved in a file that is included in compilation
typescriptStandardLibrary :: Text
typescriptStandardLibrary =
  T.decodeUtf8 $(makeRelativeToProject "static/backend/typescript/stdlib.ts" >>= embedFile)

-- these are saved in a file that is included in compilation
esModulesJSStandardLibrary :: Text
esModulesJSStandardLibrary =
  T.decodeUtf8 $(makeRelativeToProject "static/backend/es-modules-js/stdlib.mjs" >>= embedFile)

outputStdlib :: Backend -> Text
outputStdlib Typescript = typescriptStandardLibrary
outputStdlib ESModulesJS = esModulesJSStandardLibrary

indexOutputFilename :: (Printer hash) => Backend -> hash -> Text
indexOutputFilename ESModulesJS exprHash =
  "index-" <> prettyPrint exprHash <> ".mjs"
indexOutputFilename Typescript exprHash =
  "index-" <> prettyPrint exprHash <> ".ts"

symlinkedOutputPath :: Backend -> FilePath
symlinkedOutputPath ESModulesJS =
  "output/ejs"
symlinkedOutputPath Typescript =
  "output/ts"

zipFileOutputPath :: Backend -> FilePath
zipFileOutputPath _ = "./output/zip"

fileExtension :: Backend -> Text
fileExtension Typescript = ".ts"
fileExtension _ = ""

storeExprFilename :: (Printer hash) => Backend -> hash -> Text
storeExprFilename ESModulesJS hash' =
  "ejs-" <> prettyPrint hash' <> ".mjs"
storeExprFilename Typescript hash' =
  "ts-" <> prettyPrint hash'

stdlibFilename :: Backend -> Text
stdlibFilename Typescript = "ts-stdlib"
stdlibFilename ESModulesJS = "ejs-stdlib.mjs"
