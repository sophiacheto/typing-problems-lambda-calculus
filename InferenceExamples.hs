import TypeInference

t1 = infer (Lambda 'x' (Var 'x'))
-- ([],Arrow (VarType 'A') (VarType 'A'))

t2 = infer (Lambda 'x' (Var 'y'))
-- ([('y',VarType 'B')],Arrow (VarType 'A') (VarType 'B'))

t3 = infer (Lambda 'x' (Lambda 'y' (Var 'x')))
-- ([],Arrow (VarType 'B') (Arrow (VarType 'A') (VarType 'B')))

t4 = infer (App (Lambda 'f' (Lambda 'x' (App (Var 'f') (Var 'x')))) (Lambda 'y' (App (Var 'y') (Var 'z'))))
-- ([('z',VarType 'E')],Arrow (Arrow (VarType 'E') (VarType 'F')) (VarType 'F'))