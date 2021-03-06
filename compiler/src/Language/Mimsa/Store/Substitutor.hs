{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Language.Mimsa.Store.Substitutor where

import Control.Monad (join)
import Control.Monad.Trans.State.Lazy
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Language.Mimsa.Store.ExtractTypes
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Scope
import Language.Mimsa.Types.Store
import Language.Mimsa.Types.SubstitutedExpression
import Language.Mimsa.Types.Swaps

-- this turns StoreExpressions back into expressions by substituting their
-- variables for the deps passed in
--
-- like the typechecker, as we go though, we replace the names of variables with
-- var0, var1, var2 etc so we don't have to care about scoping or collisions
--
-- we'll also store what our substitutions were for errors sake

data SubsState ann = SubsState
  { subsSwaps :: Swaps,
    subsScope :: Scope ann,
    subsCounter :: Int
  }

type App ann = State (SubsState ann)

substitute ::
  (Monoid ann, Ord ann) =>
  Store ann ->
  StoreExpression ann ->
  SubstitutedExpression ann
substitute store' storeExpr =
  let startingState = SubsState mempty mempty 0
      ((_, expr'), SubsState swaps' scope' _) =
        runState
          (doSubstitutions store' storeExpr)
          startingState
   in SubstitutedExpression
        swaps'
        expr'
        scope'

-- get the list of deps for this expression, turn from hashes to StoreExprs
doSubstitutions ::
  (Monoid ann, Ord ann) =>
  Store ann ->
  StoreExpression ann ->
  App ann (Changed, Expr Variable ann)
doSubstitutions store' (StoreExpression expr bindings' tBindings) = do
  newScopes <- traverse (addDepToScope store') (getExprPairs store' bindings')
  addScope $ mconcat (fst <$> newScopes)
  let changed = mconcat (snd <$> newScopes)
  expr' <- mapVar changed expr
  dtList <- toDataTypeList <$> resolveTypesRecursive store' tBindings
  let dtExpr = addDataTypesToExpr expr' dtList
  pure (changed, dtExpr)

------------ type stuff

-- why doesn't this exist already
lookupStoreItem :: Store ann -> ExprHash -> Maybe (StoreExpression ann)
lookupStoreItem (Store s') hash' = M.lookup hash' s'

-- | recursively fetch types mentioned by type expressions
resolveTypesRecursive ::
  (Ord ann) =>
  Store ann ->
  TypeBindings ->
  App ann [StoreExpression ann]
resolveTypesRecursive store' typeBindings = do
  let storeExprs' = resolveTypes store' typeBindings
  let newTypeBindings =
        mconcat $ S.toList (S.map storeTypeBindings storeExprs')
  nextGen <-
    if S.null storeExprs'
      then pure mempty
      else resolveTypesRecursive store' newTypeBindings
  pure $ S.toList storeExprs' <> nextGen

-- find all types needed by our expression in the store
resolveTypes :: (Ord ann) => Store ann -> TypeBindings -> Set (StoreExpression ann)
resolveTypes store' (TypeBindings tBindings) =
  let typeLookup (_, hash) = case lookupStoreItem store' hash of
        Just sExpr -> S.singleton sExpr
        _ -> mempty -- TODO: are we swallowing the error here and should this all operate in ExceptT?
      manySets = fmap typeLookup (M.toList tBindings)
   in mconcat manySets

toDataTypeList :: (Ord ann) => [StoreExpression ann] -> [DataType ann]
toDataTypeList sExprs =
  let getDts sExpr = extractDataTypes (storeExpression sExpr)
   in S.toList $ mconcat (getDts <$> sExprs)

-- add required type declarations into our Expr
addDataTypesToExpr ::
  (Monoid ann) =>
  Expr var ann ->
  [DataType ann] ->
  Expr var ann
addDataTypesToExpr =
  foldr (MyData mempty)

--------------

addDepToScope ::
  (Monoid ann, Ord ann) =>
  Store ann ->
  (Name, StoreExpression ann) ->
  App ann (Scope ann, Changed)
addDepToScope store' (name, storeExpr') = do
  (chg, expr') <- doSubstitutions store' storeExpr'
  maybeKey <- scopeExists expr'
  var <- case maybeKey of
    Just existingKey -> pure existingKey
    Nothing -> getNextVarName name
  let changed = addChange name var chg
  pure (Scope (M.singleton var expr'), changed)

-- if the expression we're already saving is in the scope
-- that's the name we want to use
scopeExists :: (Eq ann) => Expr Variable ann -> App ann (Maybe Variable)
scopeExists var = do
  (Scope scope') <- gets subsScope
  pure (mapKeyFind (var ==) scope')

addScope :: Scope ann -> App ann ()
addScope scope' =
  modify (\s -> s {subsScope = subsScope s <> scope'})

addSwap :: Name -> Variable -> App ann ()
addSwap old new =
  modify (\s -> s {subsSwaps = subsSwaps s <> M.singleton new old})

mapKeyFind :: (a -> Bool) -> Map k a -> Maybe k
mapKeyFind pred' map' = case M.toList (M.filter pred' map') of
  [] -> Nothing
  ((k, _) : _) -> pure k

-- if we've already made up a name for this var, return that
findInSwaps :: Name -> App ann (Maybe Variable)
findInSwaps name = do
  swaps' <- gets subsSwaps
  pure (mapKeyFind (name ==) swaps')

getExprPairs :: Store ann -> Bindings -> [(Name, StoreExpression ann)]
getExprPairs (Store items') (Bindings bindings') = join $ do
  (name, hash) <- M.toList bindings'
  case M.lookup hash items' of
    Just item -> pure [(name, item)]
    _ -> pure []

-- get a new name for a var, changing it's reference in Scope and adding it to
-- Swaps list
-- we don't do this for variables introduced by
-- lambdas
getNextVarName ::
  Name ->
  App ann Variable
getNextVarName name =
  do
    nextName <- NumberedVar <$> nextNum
    addSwap name nextName
    pure nextName

keepName ::
  Name -> App ann Variable
keepName name = do
  let nextName = NamedVar name
  addSwap name nextName
  pure nextName

nextNum :: App ann Int
nextNum = do
  p <- gets subsCounter
  modify (\s -> s {subsCounter = p + 1})
  pure p

-- convert name to variable
nameToVar :: Changed -> Name -> Variable
nameToVar chg n = case fromChange chg n of
  Just var -> var -- we've allocated this already
  _ -> NamedVar n -- this is what we did before, not sure about it

-- this is opposite to Swaps, and allows the meaning of a variable to be
-- changed by scoping, hence unique key is name
-- we pass these around manually rather than putting them in state
-- so that the scoping dies when we're no longer down the relevant sub-tree
newtype Changed = Changed {getChanged :: Map Name Variable}
  deriving newtype (Semigroup, Monoid)

addChange :: Name -> Variable -> Changed -> Changed
addChange k v (Changed changes) = Changed (M.singleton k v <> changes)

fromChange :: Changed -> Name -> Maybe Variable
fromChange (Changed changes) n = M.lookup n changes

-- step through Expr, replacing vars with numbered variables
mapVar ::
  (Eq ann, Monoid ann) =>
  Changed ->
  Expr Name ann ->
  App ann (Expr Variable ann)
mapVar chg (MyLambda ann name body) = do
  -- here we introduce new vars so we give them nums to avoid collisions
  var <- getNextVarName name
  MyLambda ann var <$> mapVar (addChange name var chg) body
mapVar chg (MyVar ann name) =
  pure $ MyVar ann (nameToVar chg name)
mapVar chg (MyLet ann name expr' body) = do
  var <- getNextVarName name
  let withChange = addChange name var chg
  MyLet ann var
    <$> mapVar withChange expr' <*> mapVar withChange body
mapVar chg (MyLetPattern ann pat body expr) = do
  (newPat, newChg) <- mapPatternVar chg pat
  MyLetPattern ann newPat <$> mapVar newChg body <*> mapVar newChg expr
mapVar chg (MyInfix ann op a b) =
  MyInfix ann op <$> mapVar chg a <*> mapVar chg b
mapVar chg (MyRecordAccess ann a name) =
  MyRecordAccess ann
    <$> mapVar chg a <*> pure name
mapVar chg (MyApp ann a b) = MyApp ann <$> mapVar chg a <*> mapVar chg b
mapVar chg (MyIf ann a b c) = MyIf ann <$> mapVar chg a <*> mapVar chg b <*> mapVar chg c
mapVar chg (MyPair ann a b) = MyPair ann <$> mapVar chg a <*> mapVar chg b
mapVar chg (MyRecord ann map') = do
  map2 <- traverse (mapVar chg) map'
  pure (MyRecord ann map2)
mapVar chg (MyArray ann map') = do
  map2 <- traverse (mapVar chg) map'
  pure (MyArray ann map2)
mapVar _ (MyLiteral ann a) = pure (MyLiteral ann a)
mapVar chg (MyData ann dt b) =
  MyData ann dt <$> mapVar chg b
mapVar _ (MyConstructor ann name) = pure (MyConstructor ann name)
mapVar chg (MyConsApp ann fn var) = MyConsApp ann <$> mapVar chg fn <*> mapVar chg var
mapVar chg (MyPatternMatch ann expr' patterns) = do
  let mapVarPair (pat, expr'') = do
        (newPat, newChg) <- mapPatternVar chg pat
        newExpr <- mapVar newChg expr''
        pure (newPat, newExpr)
  patterns' <- traverse mapVarPair patterns
  MyPatternMatch ann <$> mapVar chg expr' <*> pure patterns'
mapVar _ (MyTypedHole ann a) = pure $ MyTypedHole ann a
mapVar chg (MyDefineInfix ann infixOp bindName expr) =
  MyDefineInfix
    ann
    infixOp
    (nameToVar chg bindName)
    <$> mapVar chg expr

mapPatternVar ::
  (Eq ann, Monoid ann) =>
  Changed ->
  Pattern Name ann ->
  App ann (Pattern Variable ann, Changed)
mapPatternVar chg (PVar ann name) = do
  -- we don't change the name so we can still detect duplicate patterns
  var <- getNextVarName name
  let pat = PVar ann var
  pure (pat, addChange name var chg)
mapPatternVar chg (PConstructor ann name more) = do
  subPatterns <- traverse (mapPatternVar chg) more
  let pat = PConstructor ann name (fst <$> subPatterns)
  let newChg = mconcat (snd <$> subPatterns) <> chg
  pure (pat, newChg)
mapPatternVar chg (PPair ann a b) = do
  (pA, ch1) <- mapPatternVar chg a
  (pB, ch2) <- mapPatternVar (ch1 <> chg) b
  pure (PPair ann pA pB, ch1 <> ch2 <> chg)
mapPatternVar chg (PRecord ann items) = do
  newMap <- traverse (mapPatternVar chg) items
  let pat = PRecord ann (fst <$> newMap)
  let newChg = mconcat (M.elems (snd <$> newMap)) <> chg
  pure (pat, newChg)
mapPatternVar chg (PArray ann as spread) = do
  newMap <- traverse (mapPatternVar chg) as
  (newSpread, chg1) <- mapSpreadVar chg spread
  let pat = PArray ann (fst <$> newMap) newSpread
  let chg2 = mconcat (snd <$> newMap) <> chg1 <> chg
  pure (pat, chg2)
mapPatternVar chg (PWildcard ann) = pure (PWildcard ann, chg)
mapPatternVar chg (PLit ann a) = pure (PLit ann a, chg)
mapPatternVar chg (PString ann a as) = do
  (pA, ch1) <- mapStringPart chg a
  (pAs, ch2) <- mapStringPart chg as
  pure (PString ann pA pAs, ch1 <> ch2 <> chg)

mapSpreadVar ::
  Changed ->
  Spread Name ann ->
  App ann (Spread Variable ann, Changed)
mapSpreadVar chg NoSpread =
  pure (NoSpread, chg)
mapSpreadVar chg (SpreadWildcard ann) =
  pure (SpreadWildcard ann, chg)
mapSpreadVar chg (SpreadValue ann name) = do
  var <- getNextVarName name
  pure (SpreadValue ann var, addChange name var chg)

mapStringPart :: Changed -> StringPart Name ann -> App ann (StringPart Variable ann, Changed)
mapStringPart chg (StrWildcard ann) =
  pure (StrWildcard ann, chg)
mapStringPart chg (StrValue ann name) = do
  var <- getNextVarName name
  pure (StrValue ann var, addChange name var chg)
