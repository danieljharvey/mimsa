{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Smol.Backend.IR.FromExpr.Expr
  ( irFromModule,
    fromExpr,
    FromExprState (..),
    getConstructorNumber,
  )
where

import Control.Monad ((>=>))
import Control.Monad.State
import Control.Monad.Writer
import Data.Bifunctor
import Data.Foldable (foldl', toList, traverse_)
import qualified Data.List.NonEmpty as NE
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Smol.Backend.IR.FromExpr.DataTypes
import qualified Smol.Backend.IR.FromExpr.Helpers as Compile
import Smol.Backend.IR.FromExpr.Pattern
import Smol.Backend.IR.FromExpr.Type
import Smol.Backend.IR.FromExpr.Types
import Smol.Backend.IR.IRExpr
import Smol.Backend.Types.GetPath
import Smol.Backend.Types.PatternPredicate
import Smol.Core.Helpers
import Smol.Core.Modules.Types (Module (..), TopLevelExpression (..))
import Smol.Core.Typecheck (flattenConstructorApplication, getTypeAnnotation)
import Smol.Core.Typecheck.Shared (getExprAnnotation)
import Smol.Core.Types.Constructor
import Smol.Core.Types.DataType
import Smol.Core.Types.Expr
import Smol.Core.Types.Identifier
import Smol.Core.Types.Op
import Smol.Core.Types.Prim
import Smol.Core.Types.ResolvedDep
import Smol.Core.Types.Type
import Smol.Core.Types.TypeName

irPrintInt :: IRExtern
irPrintInt =
  IRExtern
    { ireName = "printint",
      ireArgs = [IRInt32],
      ireReturn = IRInt32
    }

irPrintBool :: IRExtern
irPrintBool =
  IRExtern
    { ireName = "printbool",
      ireArgs = [IRInt2],
      ireReturn = IRInt32
    }

irPrintString :: IRExtern
irPrintString =
  IRExtern
    { ireName = "printstring",
      ireArgs = [IRPointer IRInt8],
      ireReturn = IRInt32
    }

irStringConcat :: IRExtern
irStringConcat =
  IRExtern
    { ireName = "stringconcat",
      ireArgs = [IRPointer IRInt8, IRPointer IRInt8],
      ireReturn = IRPointer IRInt8
    }

irStringEquals :: IRExtern
irStringEquals =
  IRExtern
    { ireName = "stringequals",
      ireArgs = [IRPointer IRInt8, IRPointer IRInt8],
      ireReturn = IRInt2
    }

getPrinter ::
  (Show ann, Show (dep Identifier), Show (dep TypeName)) =>
  Type dep ann ->
  IRExtern
getPrinter (TPrim _ TPInt) = irPrintInt
getPrinter (TPrim _ TPBool) = irPrintBool
getPrinter (TLiteral _ (TLBool _)) = irPrintBool
getPrinter (TLiteral _ (TLInt _)) = irPrintInt
getPrinter (TLiteral _ (TLString _)) = irPrintString
getPrinter other = error ("could not find a printer for type " <> show other)

getPrintFuncName ::
  ( Show ann,
    Show (dep Identifier),
    Show (dep TypeName)
  ) =>
  Type dep ann ->
  IRFunctionName
getPrintFuncName ty =
  case getPrinter ty of
    (IRExtern n _ _) -> n

getPrintFuncType ::
  ( Show ann,
    Show (dep Identifier),
    Show (dep TypeName)
  ) =>
  Type dep ann ->
  IRType
getPrintFuncType ty =
  case getPrinter ty of
    (IRExtern _ fnArgs fnReturn) -> IRFunctionType fnArgs fnReturn

getFreshName :: (MonadState (FromExprState ann) m) => String -> m String
getFreshName prefix = do
  current <- gets fesFreshInt
  modify (\s -> s {fesFreshInt = current + 1})
  pure (prefix <> show current)

getFreshFunctionName :: (MonadState (FromExprState ann) m) => m IRFunctionName
getFreshFunctionName = IRFunctionName <$> getFreshName "function"

getFreshClosureName :: (MonadState (FromExprState ann) m) => m IRIdentifier
getFreshClosureName = IRIdentifier <$> getFreshName "closure"

addVar ::
  (MonadState (FromExprState ann) m) =>
  IRIdentifier ->
  IRExpr ->
  m ()
addVar ident expr =
  modify (\s -> s {fesVars = fesVars s <> M.singleton ident expr})

-- | turn a Smol module into an IR one
-- no considerations for name collisions etc
irFromModule :: (Show ann) => Module ResolvedDep (Type ResolvedDep ann) -> IRModule
irFromModule myModule =
  let mainFunc = case M.lookup "main" (moExpressions myModule) of
        Just expr -> expr
        Nothing -> error "expected a main function"
      otherFuncs =
        M.delete "main" (moExpressions myModule)
      dataTypes =
        M.fromList
          . fmap (bimap LocalDefinition (fmap getTypeAnnotation))
          . M.toList
          $ moDataTypes myModule
   in IRModule $
        [ IRExternDef $ getPrinter (getExprAnnotation $ tleExpr mainFunc),
          IRExternDef irStringConcat,
          IRExternDef irStringEquals -- we should dynamically include these once we get a lot of stdlib helpers
        ]
          <> modulePartsFromExpr dataTypes (tleExpr <$> otherFuncs) (tleExpr mainFunc)

fromPrim :: (Monad m) => Prim -> m IRExpr
fromPrim (PInt i) = pure $ IRPrim (IRPrimInt32 i)
fromPrim (PBool b) = pure $ IRPrim (IRPrimInt2 b)
fromPrim PUnit = pure $ IRPrim (IRPrimInt2 False) -- Unit is represented the same as False
fromPrim (PString txt) = pure $ IRString txt

fromInfix ::
  (Show ann, MonadState (FromExprState ann) m, MonadWriter (Map IRIdentifier IRExpr) m) =>
  Op ->
  Expr ResolvedDep (Type ResolvedDep ann) ->
  Expr ResolvedDep (Type ResolvedDep ann) ->
  m IRExpr
fromInfix OpAdd a b = do
  irA <- fromExpr a
  irB <- fromExpr b
  if Compile.isStringType (getExprAnnotation a)
    then
      let (IRExtern fnName fnArgs fnReturn) = irStringConcat
       in pure $ IRApply (IRFunctionType fnArgs fnReturn) (IRFuncPointer fnName) [irA, irB]
    else pure (IRInfix IRAdd irA irB)
fromInfix OpEquals a b = do
  irA <- fromExpr a
  irB <- fromExpr b
  if Compile.isStringType (getExprAnnotation a)
    then
      let (IRExtern fnName fnArgs fnReturn) = irStringEquals
       in pure $ IRApply (IRFunctionType fnArgs fnReturn) (IRFuncPointer fnName) [irA, irB]
    else pure (IRInfix IREquals irA irB)

functionReturnType :: IRType -> ([IRType], IRType)
functionReturnType (IRStruct [IRPointer (IRFunctionType args ret), _]) =
  (args, ret)
functionReturnType other = error ("non-function " <> show other)

fromExpr ::
  ( Show ann,
    MonadState (FromExprState ann) m,
    MonadWriter (Map IRIdentifier IRExpr) m
  ) =>
  Expr ResolvedDep (Type ResolvedDep ann) ->
  m IRExpr
fromExpr (EPrim _ prim) = fromPrim prim
fromExpr (EInfix _ op a b) = fromInfix op a b
fromExpr (EAnn _ _ inner) = fromExpr inner
fromExpr (EIf ty predExpr thenExpr elseExpr) = do
  irPred <- fromExpr predExpr
  irThen <- fromExpr thenExpr
  irElse <- fromExpr elseExpr
  responseType <- fromType ty
  pure $
    IRMatch
      irPred
      responseType
      ( NE.fromList
          [ IRMatchCase
              { irmcType = IRInt2,
                irmcPatternPredicate = [PathEquals (GetPath [] GetValue) (IRPrim $ IRPrimInt2 True)],
                irmcGetPath = mempty,
                irmcExpr = irThen
              },
            IRMatchCase
              { irmcType = IRInt2,
                irmcPatternPredicate = [PathEquals (GetPath [] GetValue) (IRPrim $ IRPrimInt2 False)],
                irmcGetPath = mempty,
                irmcExpr = irElse
              }
          ]
      )
fromExpr (EPatternMatch ty matchExpr pats) = do
  irMatch <- fromExpr matchExpr
  let withPat (p, pExpr) = do
        irExpr <- fromExpr pExpr
        preds <- predicatesFromPattern fromPrim p
        destructured <- destructurePattern fromIdentifier p
        dt <- patternTypeInMemory p
        pure $
          IRMatchCase
            { irmcType = fromDataTypeInMemory dt,
              irmcPatternPredicate = preds,
              irmcGetPath = destructured,
              irmcExpr = irExpr
            }
  irPats <- traverse withPat pats
  responseType <- fromType ty
  pure $
    IRMatch
      irMatch
      responseType
      irPats
fromExpr (ELambda ty ident body) =
  closureFromExpr ty (Compile.resolveIdentifier ident) body
fromExpr (EApp ty fn val) =
  appFromExpr ty fn val
fromExpr (EVar ty v@(LocalDefinition var)) = do
  let irIdentifier = fromIdentifier $ Compile.resolveIdentifier v
  irType <- fromType ty
  case ty of
    TFunc _ _ arg ret -> do
      irRet <- fromType ret
      irArg <- fromType arg
      let envType = IRStruct []
          functionType = IRFunctionType [irArg, envType] irRet
          closureType = IRStruct [IRPointer functionType, envType]

      -- ir puts the function in a closure with no env
      let irExpr =
            IRInitialiseDataType
              (IRAlloc closureType)
              closureType
              closureType
              [ IRSetTo
                  [0]
                  (IRPointer functionType)
                  (IRFuncPointer (functionNameFromIdentifier var))
              ]

      tell (M.singleton irIdentifier irExpr)
      pure (IRVar irIdentifier)
    _ -> do
      -- no args funcs are like thunked values
      let irExpr = IRApply (IRFunctionType [] irType) (IRFuncPointer (functionNameFromIdentifier var)) []
      tell (M.singleton irIdentifier irExpr)
      pure (IRVar irIdentifier)
fromExpr (EVar _ var) =
  pure (IRVar $ fromIdentifier $ Compile.resolveIdentifier var)
fromExpr (ETuple ty tHead tTail) = do
  statements <-
    traverseInd
      ( \expr i -> do
          irExpr <- fromExpr expr
          exprType <- fromType (getExprAnnotation expr)
          pure $ IRSetTo [i] exprType irExpr
      )
      ([tHead] <> NE.toList tTail)
  structType <- fromType ty
  pure $
    IRInitialiseDataType
      (IRAlloc structType)
      structType
      structType
      statements
fromExpr (ELet _ ident expr body) = do
  irExpr <- fromExpr expr
  addVar (fromIdentifier (Compile.resolveIdentifier ident)) irExpr -- remember pls
  irBody <- fromExpr body
  pure (IRLet (fromIdentifier (Compile.resolveIdentifier ident)) irExpr irBody)
fromExpr (EConstructor ty constructor) = do
  tyResult <- Compile.flattenConstructorType ty
  case tyResult of
    -- genuine enum, return number
    (_typeName, []) -> getConstructorNumber (Compile.resolveConstructor constructor)
    (_typeName, _) -> do
      (structType, specificStructType) <-
        bimap
          fromDataTypeInMemory
          fromDataTypeInMemory
          <$> constructorTypeInMemory ty (Compile.resolveConstructor constructor)

      -- get number for constructor
      consNum <- getConstructorNumber (Compile.resolveConstructor constructor)

      let setConsNum = IRSetTo [0] IRInt32 consNum

      pure $
        IRInitialiseDataType
          (IRAlloc structType)
          specificStructType
          structType
          [setConsNum]
fromExpr (EArray ty items) = do
  irType <- fromType ty
  let setCount = IRSetTo [0] IRInt32 (IRPrim $ IRPrimInt32 $ fromIntegral $ length items)
  setItems <-
    traverseInd
      ( \item i -> do
          tyItem <- fromType (getExprAnnotation item)
          irItem <- fromExpr item
          pure $ IRSetTo [1, i] tyItem irItem
      )
      (toList items)
  pure $
    IRInitialiseDataType
      (IRAlloc irType)
      irType
      irType
      ([setCount] <> setItems)
fromExpr expr = error ("fuck: " <> show expr)

-- | given an env type, put all it's items in scope
-- replaces "a" with a reference it's position in scope
bindingsFromEnv :: Map (ResolvedDep Identifier) (Type ResolvedDep ann) -> IRExpr -> IRExpr
bindingsFromEnv env inner =
  foldr
    ( \(ident, i) irExpr ->
        swapVar (fromIdentifier (Compile.resolveIdentifier ident)) (IRStructPath [i] (IRVar "env")) irExpr
    )
    inner
    (zip (M.keys env) [0 ..])

swapVar :: IRIdentifier -> IRExpr -> IRExpr -> IRExpr
swapVar target replace =
  go
  where
    go (IRVar a) | a == target = replace
    go other = mapIRExpr go other

mapIRExpr :: (IRExpr -> IRExpr) -> IRExpr -> IRExpr
mapIRExpr _ (IRVar a) = IRVar a
mapIRExpr _ (IRString txt) = IRString txt
mapIRExpr _ (IRAlloc ty) = IRAlloc ty
mapIRExpr _ (IRPrim p) = IRPrim p
mapIRExpr f (IRInfix op a b) = IRInfix op (f a) (f b)
mapIRExpr f (IRApply ty fn arg) = IRApply ty (f fn) (f <$> arg)
mapIRExpr f (IRLet ident expr rest) =
  IRLet ident (f expr) (f rest)
mapIRExpr f (IRStructPath as var) =
  IRStructPath as (f var)
mapIRExpr _ (IRFuncPointer p) = IRFuncPointer p
mapIRExpr f (IRMatch expr ty pats) =
  IRMatch (f expr) ty ((\(IRMatchCase a b c irExpr) -> IRMatchCase a b c (f irExpr)) <$> pats)
mapIRExpr f (IRStatements as rest) =
  IRStatements as (f rest)
mapIRExpr f (IRPointerTo a b) =
  IRPointerTo a (f b)
mapIRExpr f (IRInitialiseDataType input a b args) =
  let mapSetTo (IRSetTo path ty expr) = IRSetTo path ty (f expr)
   in IRInitialiseDataType (f input) a b (mapSetTo <$> args)

closureFromExpr ::
  ( MonadState (FromExprState ann) m,
    MonadWriter (Map IRIdentifier IRExpr) m,
    Show ann
  ) =>
  Type ResolvedDep ann ->
  Identifier ->
  Expr ResolvedDep (Type ResolvedDep ann) ->
  m IRExpr
closureFromExpr ty ident body = do
  irType <- fromType ty
  let (argTypes, retType) = functionReturnType irType
  let argType = case argTypes of
        (a : _) -> a
        _ -> error "why don't we have any args to this function?"
  let envArgs = case ty of
        TFunc _ env _ _ -> env
        _ -> error "type is not lambda wtf"

  funcName <- getFreshFunctionName

  irBody <- fromExpr body
  envType <- typeFromEnv envArgs

  modulePart <- do
    pure
      ( IRFunctionDef
          ( IRFunction
              { irfName = funcName,
                irfArgs = [(argType, fromIdentifier ident), (envType, "env")],
                irfReturn = retType,
                irfBody =
                  [ IRRet retType (bindingsFromEnv envArgs irBody)
                  ]
              }
          )
      )

  pushModulePart modulePart

  let functionType = IRFunctionType [argType, envType] retType
      closureType = IRStruct [IRPointer functionType, envType]

  envStatements <- structFromEnv envArgs

  pure $
    IRInitialiseDataType
      (IRAlloc closureType)
      closureType
      closureType
      ( [ IRSetTo
            [0]
            (IRPointer functionType)
            (IRFuncPointer funcName)
        ]
          <> envStatements
      )

-- given an `env` value, capture all the vars from the environment to put in
-- the closure
structFromEnv ::
  ( MonadState (FromExprState ann) m,
    Show ann
  ) =>
  Map (ResolvedDep Identifier) (Type ResolvedDep ann) ->
  m [IRSetTo]
structFromEnv env =
  traverseInd
    ( \(ident, ty) i -> do
        irType <- fromType ty
        let irVal = IRVar (fromIdentifier (Compile.resolveIdentifier ident))
        pure (IRSetTo [1, i] irType irVal)
    )
    (M.toList env)

-- | applying `1` to `Just`, in the literal `Just 1` for instance
constructorAppFromExpr ::
  ( MonadState (FromExprState ann) m,
    MonadWriter (Map IRIdentifier IRExpr) m,
    Show ann
  ) =>
  Type ResolvedDep ann ->
  Constructor ->
  [Expr ResolvedDep (Type ResolvedDep ann)] ->
  m IRExpr
constructorAppFromExpr ty constructor cnArgs = do
  -- the constructor case, build up everything we need pls
  (structType, specificStructType) <-
    bimap
      fromDataTypeInMemory
      fromDataTypeInMemory
      <$> constructorTypeInMemory ty constructor

  -- get number for constructor
  consNum <- getConstructorNumber constructor

  let setConsNum = IRSetTo [0] IRInt32 consNum

  statements <-
    traverseInd
      ( \expr i -> do
          irExpr <- fromExpr expr
          exprType <- fromType (getExprAnnotation expr)
          pure $
            IRSetTo
              [i + 1]
              exprType
              irExpr
      )
      cnArgs

  pure $
    IRInitialiseDataType
      (IRAlloc structType)
      specificStructType
      structType
      ([setConsNum] <> statements)

-- | application could be function application or constructor application
-- first, we need to deal with nested `app` around a constructor and flatten
-- that into something ok
appFromExpr ::
  ( Show ann,
    MonadState (FromExprState ann) m,
    MonadWriter (Map IRIdentifier IRExpr) m
  ) =>
  Type ResolvedDep ann ->
  Expr ResolvedDep (Type ResolvedDep ann) ->
  Expr ResolvedDep (Type ResolvedDep ann) ->
  m IRExpr
appFromExpr ty fn val = do
  case flattenConstructorApplication (EApp ty fn val) of
    Just (constructor, cnArgs) ->
      constructorAppFromExpr ty (Compile.resolveConstructor constructor) cnArgs
    Nothing -> do
      -- regular function application (`id True` for instance)
      irFn <- fromExpr fn
      irVal <- fromExpr val
      fnType <- fromType (getExprAnnotation fn)
      closureName <- getFreshClosureName

      -- arguably we could look into trashing the env
      -- where it's empty but for now let's keep this easier
      pure
        ( IRLet
            closureName
            irFn
            ( IRApply
                fnType
                (IRStructPath [0] irFn)
                [ irVal,
                  IRPointerTo [1] (IRVar closureName)
                ]
            )
        )

fromIdentifier :: Identifier -> IRIdentifier
fromIdentifier (Identifier ident) = IRIdentifier (T.unpack ident)

functionNameFromIdentifier :: Identifier -> IRFunctionName
functionNameFromIdentifier (Identifier ident) =
  IRFunctionName (T.unpack ident)

pushModulePart :: (MonadState (FromExprState ann) m) => IRModulePart -> m ()
pushModulePart part =
  modify (\s -> s {fesModuleParts = fesModuleParts s <> [part]})

fromOtherExpr ::
  (Show ann, MonadState (FromExprState ann) m) =>
  Identifier ->
  Expr ResolvedDep (Type ResolvedDep ann) ->
  m IRModulePart
fromOtherExpr name (EAnn _ _ inner) = fromOtherExpr name inner
fromOtherExpr name (ELambda ty ident body) = do
  (irBody, vars) <- runWriterT (fromExpr body)

  let (tFrom, tTo) = case ty of
        (TFunc _ _ tFrom' tTo') -> (tFrom', tTo')
        _ -> error "sdfdsf"

  irReturnType <- fromType tTo
  irArgType <- fromType tFrom

  pure
    ( IRFunctionDef
        ( IRFunction
            { irfName = functionNameFromIdentifier name,
              irfArgs =
                [ (irArgType, fromIdentifier (Compile.resolveIdentifier ident)),
                  (IRStruct [], IRIdentifier "env")
                ],
              irfReturn = irReturnType,
              irfBody = [IRRet irReturnType (addVarsToExpr vars irBody)]
            }
        )
    )
fromOtherExpr name expr = do
  (irExpr, vars) <- runWriterT (fromExpr expr)
  irReturnType <- fromType (getExprAnnotation expr)

  pure
    ( IRFunctionDef
        ( IRFunction
            { irfName = functionNameFromIdentifier name,
              irfArgs = [],
              irfReturn = irReturnType,
              irfBody = [IRRet irReturnType (addVarsToExpr vars irExpr)]
            }
        )
    )

addVarsToExpr :: Map IRIdentifier IRExpr -> IRExpr -> IRExpr
addVarsToExpr vars expr =
  foldl' (\e (ident, binding) -> IRLet ident binding e) expr (M.toList vars)

-- | given an expr, return the `main` function, as well as adding any extra
-- module Core.parts to the State
modulePartsFromExpr ::
  (Show ann) =>
  Map (ResolvedDep TypeName) (DataType ResolvedDep ann) ->
  Map Identifier (Expr ResolvedDep (Type ResolvedDep ann)) ->
  Expr ResolvedDep (Type ResolvedDep ann) ->
  [IRModulePart]
modulePartsFromExpr dataTypes otherExprs mainExpr =
  let (irMainExpr, FromExprState {fesModuleParts = otherParts}) = do
        let action = do
              traverse_
                ( uncurry fromOtherExpr
                    >=> pushModulePart
                )
                (M.toList otherExprs)
              (irExpr, mainVars) <- runWriterT (fromExpr mainExpr)
              pure $ addVarsToExpr mainVars irExpr
        runState action (FromExprState mempty dataTypes 1 mempty)
      printFuncName = getPrintFuncName (getExprAnnotation mainExpr)
      printFuncType = getPrintFuncType (getExprAnnotation mainExpr)
   in otherParts
        <> [ IRFunctionDef
               ( IRFunction
                   { irfName = "main",
                     irfArgs = [],
                     irfReturn = IRInt32,
                     irfBody =
                       [ IRDiscard (IRApply printFuncType (IRFuncPointer printFuncName) [irMainExpr]),
                         IRRet IRInt32 $ IRPrim $ IRPrimInt32 0
                       ]
                   }
               )
           ]

getConstructorNumber ::
  (MonadState (FromExprState ann) m) =>
  Constructor ->
  m IRExpr
getConstructorNumber =
  Compile.primFromConstructor >=> fromPrim
