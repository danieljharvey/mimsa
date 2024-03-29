{-# LANGUAGE DerivingStrategies #-}

module Smol.Core.Interpreter.Types.Stack (StackFrame (..), ExprData (..)) where

import Data.Map.Strict (Map)
import Smol.Core.Printer
import Smol.Core.Types.Expr
import Smol.Core.Types.Identifier
import Smol.Core.Types.ResolvedDep

newtype StackFrame ann = StackFrame
  { sfVariables :: Map (ResolvedDep Identifier) (Expr ResolvedDep (ExprData ann))
  }
  deriving stock (Eq, Ord, Show)

instance Semigroup (StackFrame ann) where
  (StackFrame varA) <> (StackFrame varB) =
    StackFrame (varA <> varB)

instance Monoid (StackFrame ann) where
  mempty = StackFrame mempty

instance Printer (StackFrame ann) where
  prettyDoc (StackFrame sfVars) = prettyDoc sfVars

-- carried around in each node when interpreting
data ExprData ann = ExprData
  { edStackFrame :: StackFrame ann,
    edIsRecursive :: Bool,
    edAnnotation :: ann
  }
  deriving stock (Eq, Ord, Show)

instance (Semigroup ann) => Semigroup (ExprData ann) where
  (ExprData sfA isRecA annA) <> (ExprData sfB isRecB annB) =
    ExprData (sfA <> sfB) (isRecA || isRecB) (annA <> annB)

instance (Monoid ann) => Monoid (ExprData ann) where
  mempty = ExprData mempty False mempty
