{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Typechecker.Exhaustiveness
  ( isExhaustive,
    redundantCases,
    validatePatterns,
    noDuplicateVariables,
    smallerListVersions,
  )
where

import Control.Monad.Except
import Control.Monad.Reader
import Data.Foldable
import Data.Functor
import Data.List (nub)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import qualified Data.Set as S
import Language.Mimsa.Printer
import Language.Mimsa.Typechecker.Environment
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Swaps
import Language.Mimsa.Types.Typechecker.Environment

validatePatterns ::
  (MonadError TypeError m, MonadReader Swaps m) =>
  Environment ->
  Annotation ->
  [Pattern Variable Annotation] ->
  m ()
validatePatterns env ann patterns = do
  traverse_ noDuplicateVariables patterns
  missing <- isExhaustive env patterns
  _ <- case missing of
    [] -> pure ()
    _ -> throwError (PatternMatchErr (MissingPatterns ann missing))
  redundant <- redundantCases env patterns
  case redundant of
    [] -> pure ()
    _ -> throwError (PatternMatchErr (RedundantPatterns ann redundant))

withSwap :: Swaps -> Variable -> Name
withSwap _ (NamedVar n) = n
withSwap swaps (NumberedVar i) =
  fromMaybe
    "unknownvar"
    (M.lookup (NumberedVar i) swaps)

noDuplicateVariables ::
  (MonadError TypeError m, MonadReader Swaps m) =>
  Pattern Variable Annotation ->
  m ()
noDuplicateVariables pat = do
  swaps <- ask
  let dupes = M.keysSet . M.filter (> 1) . M.mapKeysWith (+) (withSwap swaps) . getVariables $ pat
   in if S.null dupes
        then pure ()
        else
          throwError
            ( PatternMatchErr
                ( DuplicateVariableUse
                    (getPatternAnnotation pat)
                    dupes
                )
            )

getVariables ::
  Pattern Variable Annotation ->
  Map Variable Int
getVariables (PWildcard _) = mempty
getVariables (PLit _ _) = mempty
getVariables (PVar _ a) = M.singleton a 1
getVariables (PPair _ a b) =
  M.unionWith (+) (getVariables a) (getVariables b)
getVariables (PRecord _ as) =
  foldr (M.unionWith (+)) mempty (getVariables <$> as)
getVariables (PArray _ as spread) =
  let vars = [getSpreadVariables spread] <> (getVariables <$> as)
   in foldr (M.unionWith (+)) mempty vars
getVariables (PConstructor _ _ args) =
  foldr (M.unionWith (+)) mempty (getVariables <$> args)
getVariables (PString _ a as) =
  M.unionWith (+) (getStringPartVariables a) (getStringPartVariables as)

getSpreadVariables :: Spread Variable Annotation -> Map Variable Int
getSpreadVariables (SpreadValue _ a) = M.singleton a 1
getSpreadVariables _ = mempty

getStringPartVariables :: StringPart Variable Annotation -> Map Variable Int
getStringPartVariables (StrWildcard _) = mempty
getStringPartVariables (StrValue _ a) = M.singleton a 1

-- | given a list of patterns, return a list of missing patterns
isExhaustive ::
  (Eq var, MonadError TypeError m, Printer var, Show var) =>
  Environment ->
  [Pattern var Annotation] ->
  m [Pattern var Annotation]
isExhaustive env patterns = do
  generated <-
    mconcat
      <$> traverse (generate env) patterns
  pure $ filterMissing patterns generated

generate ::
  (MonadError TypeError m, Printer var, Show var) =>
  Environment ->
  Pattern var Annotation ->
  m [Pattern var Annotation]
generate env pat = (<>) [pat] <$> generateRequired env pat

-- | Given a pattern, generate others required for it
generateRequired ::
  (MonadError TypeError m, Printer var, Show var) =>
  Environment ->
  Pattern var Annotation ->
  m [Pattern var Annotation]
generateRequired _ (PLit _ (MyBool True)) = pure [PLit mempty (MyBool False)]
generateRequired _ (PLit _ (MyBool False)) = pure [PLit mempty (MyBool True)]
generateRequired _ (PLit _ (MyInt _)) = pure [PWildcard mempty]
generateRequired _ (PLit _ (MyString "")) = pure [PString mempty (StrWildcard mempty) (StrWildcard mempty)]
generateRequired _ (PLit _ (MyString _)) = pure [PWildcard mempty]
generateRequired env (PPair _ l r) = do
  ls <- generateRequired env l
  rs <- generateRequired env r
  let allPairs = PPair mempty <$> ls <*> rs
  pure allPairs
generateRequired env (PRecord _ items) = do
  items' <- traverse (generateRequired env) items
  pure (PRecord mempty <$> sequence items')
generateRequired env (PConstructor ann tyCon args) = do
  dt <- lookupConstructor env ann tyCon
  newFromArgs <- traverse (generateRequired env) args
  newDataTypes <- requiredFromDataType dt
  let newCons = PConstructor mempty tyCon <$> sequence newFromArgs
  pure (newCons <> newDataTypes)
generateRequired env (PArray _ items _) = do
  items' <- traverse (generateRequired env) items
  let allItems = smallerListVersions (sequence items')
  pure $ (PArray mempty <$> allItems <*> pure (SpreadWildcard mempty)) <> [PArray mempty mempty NoSpread]
generateRequired _ PString {} = pure [PLit mempty (MyString "")]
generateRequired _ _ = pure mempty

-- given a list [[1,2,3]], return [[1,2,3], [1,2], [1]]
smallerListVersions :: [[a]] -> [[a]]
smallerListVersions aas =
  let get x = case x of
        [] -> []
        (_ : as) -> get as <> [x]
   in get =<< aas

requiredFromDataType ::
  (MonadError TypeError m) =>
  DataType Annotation ->
  m [Pattern var Annotation]
requiredFromDataType (DataType _ _ cons) =
  if length cons < 2 -- if there is only one constructor don't generate more
    then pure mempty
    else do
      let new (n, as) =
            [ PConstructor
                mempty
                n
                (PWildcard mempty <$ as)
            ]
      pure $ mconcat (new <$> M.toList cons)

-- filter outstanding items
filterMissing ::
  (Eq var, Eq ann) =>
  [Pattern var ann] ->
  [Pattern var ann] ->
  [Pattern var ann]
filterMissing patterns required =
  nub $ foldr annihiliatePattern required patterns
  where
    annihiliatePattern pat remaining =
      filter
        ( not
            . annihilate
              (removeAnn pat)
            . removeAnn
        )
        remaining

removeAnn :: Pattern var ann -> Pattern var ()
removeAnn p = p $> ()

-- does left pattern satisfy right pattern?
annihilate :: (Eq var) => Pattern var () -> Pattern var () -> Bool

-- | if left is on the right, get rid
annihilate a b | a == b = True
annihilate (PWildcard _) _ = True
annihilate (PVar _ _) _ = True
annihilate (PPair _ a b) (PPair _ a' b') =
  annihilate a a' && annihilate b b'
annihilate (PRecord _ as) (PRecord _ bs) =
  let diffKeys = S.difference (M.keysSet as) (M.keysSet bs)
   in S.null diffKeys
        && do
          let allPairs = zip (M.elems as) (M.elems bs)
          foldr
            (\(a, b) keep -> keep && annihilate a b)
            True
            allPairs
annihilate (PConstructor _ tyConA argsA) (PConstructor _ tyConB argsB) =
  (tyConA == tyConB)
    && foldr
      (\(a, b) keep -> keep && annihilate a b)
      True
      (zip argsA argsB)
annihilate PString {} PString {} = True
annihilate (PPair _ a b) _ =
  isComplete a && isComplete b
annihilate (PRecord _ as) _ =
  foldr (\a total -> total && isComplete a) True as
annihilate _ _as = False

-- is this item total, as such, ie, is it always true?
isComplete :: Pattern var ann -> Bool
isComplete (PWildcard _) = True
isComplete (PVar _ _) = True
isComplete (PPair _ a b) = isComplete a && isComplete b
isComplete _ = False

redundantCases ::
  (MonadError TypeError m, Eq var, Printer var, Show var) =>
  Environment ->
  [Pattern var Annotation] ->
  m [Pattern var Annotation]
redundantCases env patterns = do
  generated <-
    mconcat
      <$> traverse (generate env) patterns
  let annihiliatePattern pat remaining =
        filter
          ( not
              . annihilate
                (removeAnn pat)
              . removeAnn
          )
          remaining
  -- add index, the first pattern is never redundant
  let patternsWithIndex = zip patterns ([0 ..] :: [Int])
  pure $
    snd $
      foldl'
        ( \(remaining, redundant) (pat, i) ->
            let rest = annihiliatePattern pat remaining
             in if length rest == length remaining && i > 0
                  then (rest, redundant <> [pat])
                  else (rest, redundant)
        )
        (generated, mempty)
        patternsWithIndex
