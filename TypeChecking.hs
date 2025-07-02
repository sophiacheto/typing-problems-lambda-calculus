module TypeChecking where

data Type = VarType Char        -- variables
          | Arrow Type Type     -- lambda
          | Prod Type Type      -- pairs
          | Nat                 -- numbers
          | Boolean             -- booleans
            deriving (Show, Eq)

data Term = Var Char          
          | Lambda (Char, Type) Term  
          | App Term Term       

          | Pair Term Term
          | Fst Term
          | Snd Term

          | Zero
          | Succ Term
          | Pred Term

          | TTrue
          | FFalse
          | IsZero Term
          | IfThen Term Term Term
          | Let Term Term Term
            deriving (Show, Eq)

type Context = [(Char, Type)]


findType :: Term -> Context -> Type
findType (Var var) cntx =  head [t | (v,t)<-cntx, v==var]

findType (Lambda (term1, type1) term2) cntx  = Arrow type1 (findType term2 ((term1, type1):cntx))

findType (App term1 term2) cntx
                                | type1 == Arrow type3 type4 && type2 == type3 = type4
                                | otherwise = error "Invalid"
                                where type1 = findType term1 cntx
                                      type2 = findType term2 cntx
                                      (Arrow type3 type4) = type1

findType (Pair term1 term2) cntx = Prod (findType term1 cntx) (findType term2 cntx)
findType (Fst term) cntx
                            | Prod t1 t2 == findType term cntx = t1
                            | otherwise = error "Invalid"
                            where (Prod t1 t2) = findType term cntx
findType (Snd term) cntx
                            | Prod t1 t2 == findType term cntx = t2
                            | otherwise = error "Invalid"
                            where (Prod t1 t2) = findType term cntx

findType Zero cntx = Nat
findType (Succ term) cntx
                            | findType term cntx == Nat = Nat
                            | otherwise = error "Invalid"
findType (Pred term) cntx
                            | findType term cntx == Nat = Nat
                            | otherwise = error "Invalid"

findType TTrue cntx = Boolean
findType FFalse cntx = Boolean
findType (IsZero term) cntx
                          | findType term cntx == Nat = Boolean
                          | otherwise = error "Invalid"

findType (IfThen condition iftrue iffalse) cntx
                                          | findType condition cntx == Boolean && t1==t2 = t1
                                          | otherwise = error "Invalid"
                                          where t1 = findType iftrue cntx
                                                t2 = findType iffalse cntx

findType (Let var val term) cntx
                                | findType var cntx == findType val cntx = findType term cntx
                                | otherwise = error "Invalid"


getType :: Term -> Context -> String 
getType term cntx = prettyPrint (findType term cntx)


prettyPrint :: Type -> String
prettyPrint (VarType char) = [char]
prettyPrint (Arrow t1 t2) = prettyPrint t1 ++ "->" ++ prettyPrint t2
prettyPrint (Prod t1 t2) = prettyPrint t1 ++ "x" ++ prettyPrint t2
prettyPrint Nat = "Nat"
prettyPrint Boolean = "Boolean"


typeCheck :: Term -> Type -> Context -> Bool
typeCheck term typeTerm cntx = typeTerm == findType term cntx