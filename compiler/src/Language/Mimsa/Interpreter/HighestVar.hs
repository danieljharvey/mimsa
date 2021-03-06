module Language.Mimsa.Interpreter.HighestVar (highestVar) where

import Data.Semigroup (Max (..))
import Language.Mimsa.ExprUtils
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Identifiers

-- interpreter starts creating new variables, so we need to check the ones we
-- have to avoid collisions
highestVar :: Expr Variable ann -> Int
highestVar expr' = max 0 $ getMax $ withMonoid getHighest expr'
  where
    getHighest (MyVar _ (NumberedVar i)) = (False, Max i)
    getHighest (MyDefineInfix _ _ (NumberedVar i) _) = (True, Max i)
    getHighest _ = (True, mempty)
