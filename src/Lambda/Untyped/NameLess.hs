module Lambda.Untyped.NameLess
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

data TermF r
  = Var Int
  | Lam r
  | App r r

termFPat :: TermF r -> Int
termFPat t = case t of
  Var _   -> 0
  Lam _   -> 1
  App _ _ -> 2

isVarF :: TermF r -> Bool
isVarF t = case t of
  Var _ -> True
  _     -> False

isLamF :: TermF r -> Bool
isLamF t = case t of
  Lam _ -> True
  _     -> False

isAppF :: TermF r -> Bool
isAppF t = case t of
  App _ _ -> True
  _       -> False

type Term = Fix TermF

isVar :: Term -> Bool
isVar = isVarF . unfix 

isLam :: Term -> Bool
isLam = isLamF . unfix

type AnnTermF info = Product TermF (Constant info)

infoF :: AnnTermF info r -> info
infoF = getConstant . π₂

isVarAF :: AnnTermF info r -> Bool
isVarAF = isVarF . π₁

isLamAF :: AnnTermF info r -> Bool
isLamAF = isLamF . π₁

isAppAF :: AnnTermF info r -> Bool
isAppAF = isAppF . π₁

type AnnTerm info = Fix (AnnTermF info)

info :: AnnTerm info -> info
info = infoF . unfix

isVarA :: AnnTerm info -> Bool
isVarA = isVarAF . unfix

isLamA :: AnnTerm info -> Bool
isLamA = isLamAF . unfix

isAppA :: AnnTerm info -> Bool
isAppA = isAppAF . unfix

type Expr         = Term
type AnnExpr info = AnnTerm info
