module PrettyPrinter
  ( printTerm  ,     -- pretty printer para terminos
    printType        -- pretty printer para tipos
  )
where

import  Common
import  Text.PrettyPrint.HughesPJ
import  Prelude hiding ((<>))

-- lista de posibles nombres para variables
vars :: [String]
vars =
  [ c : n
  | n <- "" : map show [(1 :: Integer) ..]
  , c <- ['x', 'y', 'z'] ++ ['a' .. 'w']
  ]

parensIf :: Bool -> Doc -> Doc
parensIf True  = parens
parensIf False = id

-- pretty-printer de términos

pp :: Int -> [String] -> Term -> Doc
pp ii vs (Bound k         ) = text (vs !! (ii - k - 1))
pp _  _  (Free  (Global s)) = text s

pp ii vs (i :@: c         ) = sep
  [ parensIf (isLam i || isLet i || isRec i) (pp ii vs i)
  , nest 1 (parensIf (isLam c || isApp c || isLet c || isRec c) (pp ii vs c))
  ]
pp ii vs (Lam t c) =
  text "\\"
    <> text (vs !! ii)
    <> text ":"
    <> printType t
    <> text ". "
    <> pp (ii + 1) vs c
pp ii vs (Let lt1 lt2) =
  text "let "
    <> text (vs !! ii)
    <> text " = "
    <> pp ii vs lt1
    <> text " in "
    <> pp (ii + 1) vs lt2
pp _ _ Zero = text "0"
pp ii vs (Suc lt) = text "suc " <> pp ii vs lt
pp ii vs (Rec lt1 lt2 lt3) = sep
  [ text "R" 
  , parensIf (isApp lt1 || isLam lt1 || isLet lt1) (pp ii vs lt1)
  , parensIf (isApp lt2 || isLam lt2 || isLet lt2) (pp ii vs lt2)
  , pp ii vs lt3
  ]

isLam :: Term -> Bool
isLam (Lam _ _) = True
isLam _         = False

isApp :: Term -> Bool
isApp (_ :@: _) = True
isApp _         = False

isLet :: Term -> Bool
isLet (Let _ _) = True
isLet _         = False

isRec :: Term -> Bool
isRec (Rec _ _ _) = True
isRec _           = False

-- pretty-printer de tipos
printType :: Type -> Doc
printType NatT = text "Nat"
printType EmptyT = text "E"
printType (FunT t1 t2) =
  sep [parensIf (isFun t1) (printType t1), text "->", printType t2]


isFun :: Type -> Bool
isFun (FunT _ _) = True
isFun _          = False

fv :: Term -> [String]
fv (Bound _         ) = []
fv (Free  (Global n)) = [n]
fv (t   :@: u       ) = fv t ++ fv u
fv (Lam _   u       ) = fv u
fv (Let t u         ) = fv t ++ fv u
fv (Zero            ) = []
fv (Suc u           ) = fv u
fv (Rec u v w       ) = fv u ++ fv v ++ fv w

---
printTerm :: Term -> Doc
printTerm t = pp 0 (filter (\v -> not $ elem v (fv t)) vars) t

