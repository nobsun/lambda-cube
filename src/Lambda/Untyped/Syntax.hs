module Lambda.Untyped.Syntax
  ( Fix (..)
  , Constant (..)
  , Product (..)
  , TermF (..)
  , AnnTermF
  )
   where

import Data.Functor.Foldable
import Data.Functor.Constant
import Data.Functor.Product

π₁ :: Product f g a -> f a
π₁ (Pair fa _) = fa

π₂ :: Product f g a -> g a
π₂ (Pair _ ga) = ga

-- Term

data TermF a r
  = Var a
  | Lam a r
  | App r r

termFPat :: TermF a r -> Int
termFPat t = case t of
  Var _   -> 0
  Lam _ _ -> 1
  App _ _ -> 2

isVarF :: TermF a r -> Bool
isVarF t = case t of
  Var _ -> True
  _     -> False

isLamF :: TermF a r -> Bool
isLamF t = case t of
  Lam _ _ -> True
  _       -> False

isAppF :: TermF a r -> Bool
isAppF t = case t of
  App _ _ -> True
  _       -> False

type Term a = Fix (TermF a)

isVar :: Term a -> Bool
isVar = isVarF . unfix 

isLam :: Term a -> Bool
isLam = isLamF . unfix

type AnnTermF a info = Product (TermF a) (Constant info)

infoF :: AnnTermF a info r -> info
infoF = getConstant . π₂

isVarAF :: AnnTermF a info r -> Bool
isVarAF = isVarF . π₁

isLamAF :: AnnTermF a info r -> Bool
isLamAF = isLamF . π₁

isAppAF :: AnnTermF a info r -> Bool
isAppAF = isAppF . π₁

type AnnTerm a info = Fix (AnnTermF a info)

info :: AnnTerm a info -> info
info = infoF . unfix

isVarA :: AnnTerm a info -> Bool
isVarA = isVarAF . unfix

isLamA :: AnnTerm a info -> Bool
isLamA = isLamAF . unfix

isAppA :: AnnTerm a info -> Bool
isAppA = isAppAF . unfix

type Expr         = Term Int
type AnnExpr info = AnnTerm Int info
