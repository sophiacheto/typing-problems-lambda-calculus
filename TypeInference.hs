import Data.List (delete,  intersect)

data Type = VarType Char      
          | Arrow Type Type   
            deriving (Show, Eq)

data Term = Var Char       
          | Lambda Char Term 
          | App Term Term   
            deriving (Show, Eq)

type Context = [(Char, Type)]

------- INFERENCE
infer :: Term -> (Context, Type)
infer = typeInfer []


typeInfer :: [Char] -> Term -> (Context, Type)
typeInfer typesUsed (Var v) = ([(v, VarType t)], VarType t)
                            where t = newType typesUsed
typeInfer typesUsed (Lambda ch term)
                                    | ch `occursInTerm` term = let (basis, tp') = typeInfer typesUsed term
                                                               in (erase basis ch, Arrow (findType ch basis) tp')
                                    | otherwise = let
                                                    newt = newType typesUsed
                                                    (basis, tp') = typeInfer (newt : typesUsed) term
                                                  in (basis, Arrow (VarType newt) tp')
typeInfer typesUsed (App term1 term2) = (applySubsCntx subs (basis1 ++ basis2), applySubs subs newt)
                                        where (basis1, tp1) = typeInfer typesUsed term1     -- checar typesused
                                              (basis2, tp2) = typeInfer typesUsed term2
                                              freeVar = freeVariables term1 `intersect` freeVariables term2
                                              newt = VarType (newType typesUsed)
                                              subs = unify ((tp1, Arrow tp2 newt) : (getSet freeVar basis1 basis2))


------- UNIFICATION
unify :: [(Type, Type)] -> [(Char, Type)]
unify [] = []
unify (curr:rest) = unifyTypes curr ++ unify rest

unifyTypes :: (Type, Type) -> [(Char, Type)]
unifyTypes (VarType c, t)
                    | VarType c == t = []
                    | not (c `isFreeVar` t) = [(c, t)]
                    | otherwise = error "Fail"
unifyTypes (t, VarType c) = unifyTypes (VarType c, t)
unifyTypes (Arrow a b, Arrow c d) = unifyTypes (applySubs subs a, applySubs subs c) `compose` subs
                                    where subs = unifyTypes (b, d)



------- AUX
freeVariables :: Term -> [Char]
freeVariables (Var v) = [v]
freeVariables (Lambda ch term) = delete ch (freeVariables term)
freeVariables (App term1 term2) = freeVariables term1 ++ freeVariables term2


isFreeVar :: Char -> Type -> Bool
isFreeVar ch (VarType v) = ch == v
isFreeVar ch (Arrow t1 t2) = isFreeVar ch t1 || isFreeVar ch t2


newType :: [Char] -> Char
newType used = head (filter (`notElem` used) ['A'..'Z'])


getSet :: [Char] -> Context -> Context -> [(Type, Type)]
getSet freeVar basis1 basis2 = [(findType var basis1, findType var basis2) | var <- freeVar]


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


occursInTerm :: Char -> Term -> Bool
occursInTerm ch (Var t) = ch == t
occursInTerm ch (Lambda c term) = occursInTerm ch (Var c) || occursInTerm ch term
occursInTerm ch (App t1 t2) = occursInTerm ch t1 || occursInTerm ch t2


findType :: Char -> Context -> Type
findType ch cntx = head [t | (v,t)<-cntx, v==ch]