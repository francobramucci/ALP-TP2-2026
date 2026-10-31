module Simplytyped
  ( conversion,    -- conversion a terminos localmente sin nombre
    eval,          -- evaluador
    infer,         -- inferidor de tipos
    quote          -- valores -> terminos
  )
where

import qualified Data.Map as M
import           Data.List
import           Data.Maybe
import           Prelude                 hiding ( (>>=) )
import           Text.PrettyPrint.HughesPJ      ( render )
import           PrettyPrinter
import           Common

-----------------------
-- conversion
-----------------------

-- conversion a términos localmente sin nombres
conversion :: LamTerm -> Term
conversion lt = aux lt M.empty

aux :: LamTerm -> M.Map String Int -> Term
aux (LVar s)           m = case M.lookup s m of
                                Just x  -> Bound x
                                _       -> Free (Global s)
aux (LAbs s ty lt)     m = Lam ty (aux lt (M.insert s 0 (M.map (+1) m)))
aux (LApp lt1 lt2)     m = (aux lt1 m) :@: (aux lt2 m)
aux (LLet s lt1 lt2)   m = Let (aux lt1 m) (aux lt2 (M.insert s 0 (M.map (+1) m)))
aux (LZero)            m = Zero
aux (LSuc lt)          m = Suc (aux lt m)
aux (LRec lt1 lt2 lt3) m = Rec (aux lt1 m) (aux lt2 m) (aux lt3 m)

----------------------------
--- evaluador de términos
----------------------------

-- substituye una variable por un término en otro término
sub :: Int -> Term -> Term -> Term
sub i t (Bound j) | i == j    = t
sub _ _ (Bound j) | otherwise = Bound j
sub _ _ (Free n   )           = Free n
sub i t (u   :@: v)           = sub i t u :@: sub i t v
sub i t (Lam t'  u)           = Lam t' (sub (i + 1) t u)
sub i t (Let u v  )           = Let (sub i t u) (sub (i+1) t v)
sub i t (Zero     )           = Zero
sub i t (Suc lt   )           = Suc (sub i t lt)
sub i t (Rec lt1 lt2 lt3)     = Rec (sub i t lt1) (sub i t lt2) (sub i t lt3)

-- convierte un valor en el término equivalente
quote :: Value -> Term
quote (VLam t f) = Lam t f
quote (VNum NZero) = Zero
quote (VNum (NSuc nv)) = Suc (quote (VNum nv))

-- evalúa un término en un entorno dado
-- Qué pasa si la var no está en el entorno? Lo maneja el type checker
eval :: NameEnv Value Type -> Term -> Value
eval nvs fv@(Free n)            = case lookup n nvs of
                                       Just (v, _) -> v
eval nvs (lt1 :@: lt2)          = let VLam _ alt = eval nvs lt1
                                  in eval nvs (sub 0 (quote (eval nvs lt2)) alt)
eval nvs (Lam t lt)             = VLam t lt
eval nvs (Let lt1 lt2)          = let v = quote (eval nvs lt1)
                                  in eval nvs (sub 0 v lt2)
eval nvs Zero                   = VNum NZero
eval nvs (Suc lt)               = let VNum nv = eval nvs lt
                                  in VNum (NSuc nv)
eval nvs (Rec lt1 lt2 Zero)     = eval nvs lt1
eval nvs (Rec lt1 lt2 (Suc lt)) = let nv = quote (eval nvs lt)
                                  in eval nvs ((lt2 :@: (Rec lt1 lt2 nv)) :@: nv)


----------------------
--- type checker
-----------------------

-- infiere el tipo de un término
infer :: NameEnv Value Type -> Term -> Either String Type
infer = infer' []

-- definiciones auxiliares
ret :: Type -> Either String Type
ret = Right

err :: String -> Either String Type
err = Left

(>>=)
  :: Either String Type -> (Type -> Either String Type) -> Either String Type
(>>=) v f = either Left f v
-- fcs. de error

matchError :: Type -> Type -> Either String Type
matchError t1 t2 =
  err
    $  "se esperaba "
    ++ render (printType t1)
    ++ ", pero "
    ++ render (printType t2)
    ++ " fue inferido."

notfunError :: Type -> Either String Type
notfunError t1 = err $ render (printType t1) ++ " no puede ser aplicado."

notfoundError :: Name -> Either String Type
notfoundError n = err $ show n ++ " no está definida."

-- infiere el tipo de un término a partir de un entorno local de variables y un entorno global
infer' :: Context -> NameEnv Value Type -> Term -> Either String Type
infer' c _ (Bound i) = ret (c !! i)
infer' _ e (Free  n) = case lookup n e of
                            Nothing     -> notfoundError n
                            Just (_, t) -> ret t
infer' c e (t :@: u) = infer' c e t >>= \tt -> infer' c e u >>= \tu ->
  case tt of
       FunT t1 t2 -> if (tu == t1) then ret t2 else matchError t1 tu
       _          -> notfunError tt
infer' c e (Lam t u) = infer' (t : c) e u >>= \tu -> ret $ FunT t tu
infer' c e (Let t u) = do
  t1 <- infer' c e t
  t2 <- infer' (t1:c) e u
  ret t2
infer' c e Zero      = ret NatT
infer' c e (Suc t)   = do
  tt <- infer' c e t
  case tt of
       NatT -> ret NatT
       _    -> matchError NatT tt
infer' c e (Rec t1 t2 t3) = do
  t <- infer' c e t1
  tt2 <- infer' c e t2
  case tt2 of
       et@(FunT ta (FunT NatT tb)) | ta == tb && ta == t -> do tt3 <- infer' c e t3 
                                                               if tt3 == NatT then ret t 
                                                                              else matchError NatT tt3
       _                                                 -> matchError (FunT t (FunT NatT t)) tt2
        
    












