module Frontend.Dianostic where

import Control.Monad.Error

import Frontend.Types
import Internal

type Dianostic = String

unboundVar :: (Error e, MonadError e m) => Name -> m a
unboundVar n = throwError $ strMsg $ "unbound variable " ++ n

unboundCon :: (Error e, MonadError e m) => Name -> m a
unboundCon n = throwError $ strMsg $ "unbound constructor " ++ n

unboundTypeVar :: (Error e, MonadError e m) => Name -> m a
unboundTypeVar n = throwError $ strMsg $ "unbound type variable " ++ n

unboundTypeCon :: (Error e, MonadError e m) => Name -> m a
unboundTypeCon n = throwError $ strMsg $ "unbound type constructor " ++ n

conArgsNum :: (Error e, MonadError e m) => Name -> Int -> Int -> m a
conArgsNum n expected actual =
  throwError $ strMsg $ "constructor " ++ n
                                       ++ " expects " ++ show expected
                                       ++ " argument(s), but is applied to " ++ show actual
                                       ++ " argument(s)"

typeConArgsNum :: (Error e, MonadError e m) => Name -> Int -> Int -> m a
typeConArgsNum n expected actual = 
  throwError $ strMsg $ "type constructor " ++ n
                                            ++ " expects " ++ show expected
                                            ++ " argument(s), but is applied to " ++ show actual
                                            ++ " argument(s)"

imcompatibleTypes :: (Error e, MonadError e m) => Type -> Type -> m a
imcompatibleTypes t1 t2 =
  throwError $ strMsg $ "imcompatible types " ++ show t1 ++ " and " ++ show t2
