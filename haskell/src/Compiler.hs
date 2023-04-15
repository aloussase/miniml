module Compiler where

import           Machine
import           Parser

compile :: Expr -> Frame
compile (EInt n)       = [IInt n]
compile (EBool b)      = [IBool b]
compile (EIdent ident) = [IVar ident]
compile (EPlus e1 e2)  = compile e1 ++ compile e2 ++ [IAdd]
compile (EMinus e1 e2) = compile e1 ++ compile e2 ++ [ISub]
compile (EEqual e1 e2) = compile e1 ++ compile e2 ++ [IEqual]
compile (ELess e1 e2)  = compile e1 ++ compile e2 ++ [ILess]
compile (ETimes e1 e2) = compile e1 ++ compile e2 ++ [IMult]
compile (EApp e1 e2)   = compile e1 ++ compile e2 ++ [ICall]
compile (EIf e1 e2 e3) = compile e1 ++ [IBranch (compile e2) (compile e3)]
compile (EFun ident param _ _ e) = [IClosure ident param (compile e ++ [IPopEnv])]
