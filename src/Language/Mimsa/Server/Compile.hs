{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Language.Mimsa.Server.Compile (CompileAPI, compileEndpoints) where

import qualified Data.Aeson as JSON
import qualified Data.ByteString.Lazy as LBS
import Data.Set (Set)
import Data.Swagger (NamedSchema (..), ToSchema, binarySchema, declareNamedSchema)
import Data.Text (Text)
import GHC.Generics
import qualified Language.Mimsa.Actions.Compile as Actions
import Language.Mimsa.Backend.Types
import Language.Mimsa.Backend.ZipFile
import Language.Mimsa.Server.Handlers
import Language.Mimsa.Server.Helpers
import Language.Mimsa.Server.Types
import Language.Mimsa.Types.Project
import Language.Mimsa.Types.Store
import Servant

-----

type CompileAPI =
  "compile"
    :> CompileExpression

compileEndpoints ::
  MimsaEnvironment ->
  Server CompileAPI
compileEndpoints =
  compileExpressionEndpoint

newtype CompileExpressionResponse = CompileExpressionResponse LBS.ByteString
  deriving newtype (MimeRender OctetStream)

instance ToSchema CompileExpressionResponse where
  declareNamedSchema _cer = pure (NamedSchema Nothing binarySchema)

------

data CompileExpressionRequest = CompileExpressionRequest
  { ceProjectHash :: ProjectHash,
    ceExpression :: Text
  }
  deriving (Eq, Ord, Show, Generic, JSON.FromJSON, ToSchema)

-- return type of a ZIP file download with a filename in it's header
type CompileResponse =
  ( Headers
      '[Header "Content-Disposition" String]
      CompileExpressionResponse
  )

type CompileExpression =
  "expression"
    :> ReqBody '[JSON] CompileExpressionRequest
    :> Post
         '[OctetStream]
         CompileResponse

compileExpressionEndpoint ::
  MimsaEnvironment ->
  CompileExpressionRequest ->
  Handler CompileResponse
compileExpressionEndpoint
  mimsaEnv
  (CompileExpressionRequest projectHash input) = do
    expr <- parseHandler input
    (_, (rootExprHash, exprHashes)) <-
      fromActionM mimsaEnv projectHash (Actions.compile CommonJS input expr)
    let filename = "mimsa-" <> show rootExprHash <> ".zip"
        contentDisposition = "attachment; filename=\"" <> filename <> "\""
    bs <- doCreateZipFile mimsaEnv CommonJS exprHashes rootExprHash
    pure (addHeader contentDisposition (CompileExpressionResponse bs))

doCreateZipFile ::
  MimsaEnvironment ->
  Backend ->
  Set ExprHash ->
  ExprHash ->
  Handler LBS.ByteString
doCreateZipFile mimsaEnv be exprHashes rootExprHash = do
  let mimsaCfg = mimsaConfig mimsaEnv
  archive <- handleMimsaM mimsaCfg InternalError (createZipFile be exprHashes rootExprHash)
  pure (encodeZipFile archive)