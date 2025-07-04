module TypeInference where

import Data.List (delete,  intersect)
import Data.Maybe (fromJust)

data Type = VarType Char      -- tipos
          | Arrow Type Type     -- tipo -> tipo
            deriving (Show, Eq)

data Term = Var Char          -- variaveis
          | Lambda Char Term  -- f(x)=y
          | App Term Term       -- aplicação
            deriving (Show, Eq)

type Context = [(Char, Type)]

------- INFERENCE
-- infer :: Term -> (Context, Type)
infer :: Term -> IO ()
infer term = putStrLn (prettyPrint (fst (typeInfer [] term)))


typeInfer :: [Char] -> Term -> ((Context, Type), [Char])
typeInfer typesUsed (Var v) = (([(v, VarType t)], VarType t), t : typesUsed)
                            where t = newType typesUsed
typeInfer typesUsed (Lambda ch term) = case chType of
                                    Just chT -> ((erase basis ch, Arrow chT tp'), used)
                                    Nothing -> let newt = newType used
                                                  in ((basis, Arrow (VarType newt) tp'), newt : used)
                                    where ((basis, tp'), used) = typeInfer typesUsed term
                                          chType = findType ch basis
typeInfer typesUsed (App term1 term2) = ((applySubsCntx subs (basis1 ++ basis2), applySubs subs (VarType newt)), newt : used2)
                                        where ((basis1, tp1), used1) = typeInfer typesUsed term1     
                                              ((basis2, tp2), used2) = typeInfer used1 term2
                                              freeVar = freeVariables term1 `intersect` freeVariables term2
                                              newt = newType used2
                                              subs = unify ((tp1, Arrow tp2 (VarType newt)) : (getSet freeVar basis1 basis2))



------- UNIFICATION
unify :: [(Type, Type)] -> [(Char, Type)]
unify [] = []
unify (curr:rest) = unifyTypes curr ++ unify rest

unifyTypes :: (Type, Type) -> [(Char, Type)]
unifyTypes (VarType c, t)
                    | VarType c == t = []
                    | not (c `isTypeVar` t) = [(c, t)]
                    | otherwise = error "Fail"
unifyTypes (t, VarType c) = unifyTypes (VarType c, t)
unifyTypes (Arrow a b, Arrow c d) = unifyTypes (applySubs subs a, applySubs subs c) `compose` subs
                                    where subs = unifyTypes (b, d)



------- AUX
freeVariables :: Term -> [Char]
freeVariables (Var v) = [v]
freeVariables (Lambda ch term) = delete ch (freeVariables term)
freeVariables (App term1 term2) = freeVariables term1 ++ freeVariables term2


isTypeVar :: Char -> Type -> Bool
isTypeVar ch (VarType v) = ch == v
isTypeVar ch (Arrow t1 t2) = isTypeVar ch t1 || isTypeVar ch t2


newType :: [Char] -> Char
newType used = head (filter (`notElem` used) ['A'..'Z'])


getSet :: [Char] -> Context -> Context -> [(Type, Type)]
getSet freeVar basis1 basis2 = [(fromJust (findType var basis1), fromJust (findType var basis2)) | var <- freeVar]



compose :: [(Char, Type)] -> [(Char, Type)] -> [(Char, Type)]
compose subs1 subs2 = result ++ subs1
                    where result = [ (v, applySubs subs1 t) | (v, t) <- subs2 ]  


applySubs :: [(Char, Type)] -> Type -> Type
applySubs subs (VarType ch) = subsVar subs ch
applySubs subs (Arrow term term') = Arrow (applySubs subs term) (applySubs subs term')


subsVar :: [(Char, Type)] -> Char -> Type
subsVar [] ch = VarType ch
subsVar ((old, new) : subs) ch
                    | ch == old = new
                    | otherwise = subsVar subs ch


applySubsCntx :: [(Char, Type)] -> Context -> Context
applySubsCntx subs cntx = [(v, applySubs subs t) | (v, t) <- cntx]


erase :: Context -> Char -> Context
erase cntx var = [(v,t) | (v,t)<-cntx, v/=var]


findType :: Char -> Context -> Maybe Type
findType ch cntx
            | not (null l) = Just (head l)
            | otherwise = Nothing
            where l = [t | (v,t) <- cntx, v == ch]

prettyPrint :: (Context, Type) -> String
prettyPrint (ctx, t) = "CONTEXT\n" ++ prettyCtx ctx ++ "\nTYPE: " ++ prettyType t

prettyCtx :: Context -> String
prettyCtx [] = []
prettyCtx ((chr, t) : xs) = "(" ++ [chr] ++ " : " ++ prettyType t ++ ")" ++ ['\n'] ++ prettyCtx xs

prettyType :: Type -> String
prettyType (VarType char) = [char]
prettyType (Arrow t1 t2) = prettyType t1 ++ "->" ++ prettyType t2