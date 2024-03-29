{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Builder.Polymorphic (doJobs, getMissing, Plan (..), State (..), Job, Inputs) where

import Control.Parallel.Strategies
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Set (Set)
import qualified Data.Set as S

-- a thing we want to do
data Plan k input = Plan
  { jbDeps :: Set k,
    jbInput :: input
  }
  deriving stock (Eq, Ord, Show)

-- how we're going to do it
type Job m k input output = Map k output -> input -> m output

type Inputs k input = Map k (Plan k input)

-- state of the job
data State k input output = State
  { stInputs :: Inputs k input,
    stOutputs :: Map k output
  }
  deriving stock (Eq, Ord, Show)

-- | one run of the builder builds everything that is currently ready, then
-- updates the state
runBuilder ::
  (Ord k, Monad m) =>
  Job m k input output ->
  State k input output ->
  m (State k input output)
runBuilder fn st = do
  -- filter out finished stuff from inputs (lets us start with cached results)
  let inputs = M.filterWithKey (\k _ -> M.notMember k (stOutputs st)) (stInputs st)
  -- get jobs we are ready to do
  let readyJobs =
        M.filter
          ( \plan ->
              let requiredKeys = jbDeps plan
               in and ((\depK -> M.member depK (stOutputs st)) <$> S.toList requiredKeys)
          )
          inputs

  -- do the jobs
  done <-
    traverse
      ( \(k, plan) -> do
          let filteredOutput =
                M.filterWithKey
                  (\depK _ -> S.member depK (jbDeps plan))
                  (stOutputs st)
          output <- fn filteredOutput (jbInput plan)
          pure (k, output)
      )
      (M.toList readyJobs)

  -- evaluate everything in parallel
  let reallyDone = done `using` parTraversable rseq

  -- remove them from inputs
  let newInputs =
        M.filterWithKey
          (\k _ -> S.notMember k (M.keysSet readyJobs))
          inputs

  -- add them to outputs
  pure (State newInputs (stOutputs st <> M.fromList reallyDone))

-- list the required deps that cannot possibly be provided (usually indicates
-- an error with implementation)
getMissing :: (Ord k) => State k input output -> Set k
getMissing (State inputs outputs) =
  let getMissingDeps (Plan deps _) =
        S.filter
          (\dep -> dep `M.notMember` inputs && dep `M.notMember` outputs)
          deps
   in mconcat (getMissingDeps <$> M.elems inputs)

-- run through a list of jobs and do them
doJobs ::
  (Ord k, Show k, Monad m, Eq input, Eq output) =>
  Job m k input output ->
  State k input output ->
  m (State k input output)
doJobs fn st = do
  let missingDeps = getMissing st
  if not (S.null missingDeps)
    then error ("Missing deps in build: " <> show missingDeps)
    else do
      newState <- runBuilder fn st
      if M.null (stInputs newState) || newState == st -- no more inputs, or there was no change (to stop infinite loop)
        then pure newState
        else doJobs fn newState
