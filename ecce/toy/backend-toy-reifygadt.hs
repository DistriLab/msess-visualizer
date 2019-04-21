{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}

import Language.Haskell.TH

data Pure

data Expr a where
  EBool :: Bool -> Expr Bool
  EPureBool :: Expr Bool -> Expr Pure
  EPureAnd :: Expr Pure -> Expr Pure -> Expr Pure
  EPureOr :: Expr Pure -> Expr Pure -> Expr Pure
  EPureNot :: Expr Pure -> Expr Pure
  EInteger :: Integer -> Expr Integer
  EIntegerNeg :: Expr Integer -> Expr Integer
  EIntegerMul :: Expr Integer -> Expr Integer -> Expr Integer
  EIntegerAdd :: Expr Integer -> Expr Integer -> Expr Integer

typeInfo = reify ''Expr
-- $(stringE . show =<< typeInfo)
