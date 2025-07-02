import TypeChecking

declaracoes = [('x', VarType 'A'), ('y', VarType 'B'), ('z', VarType 'C')]

t1 = typeCheck (Lambda ('w', VarType 'D') (Var 'w')) (Arrow (VarType 'D') (VarType 'D')) declaracoes
-- True

t2 =  typeCheck (Lambda ('w', VarType 'D') (Var 'w')) (Arrow (VarType 'B') (VarType 'B')) declaracoes
-- False

t3 = typeCheck (Lambda ('e', VarType 'E') (Pair (Var 'e') (Var 'y'))) (Arrow (VarType 'E') (Prod (VarType 'E') (VarType 'B'))) declaracoes
-- True

