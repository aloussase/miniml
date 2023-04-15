module Typechecker
(
      typeOfExpr
    , Context
    , emptyContext
    , extendContext
)
where

import           Control.Monad        (when)
import           Control.Monad.Except (throwError)
import           Data.List.Extra      (allSame)

import           Error
import           Lexer
import           Parser


newtype Context = Context [(String, Ty)] deriving Show

emptyContext :: Context
emptyContext = Context []

lookupContext :: Context -> String -> Maybe Ty
lookupContext (Context ctx) ident = lookup ident ctx

extendContext :: Context -> String -> Ty -> Context
extendContext (Context ctx) ident ty = Context $ (ident,ty):ctx

typeOfExpr :: Context -> Posn -> Expr -> Either Error Ty
typeOfExpr _ _ (EInt _)                             = pure TInt
typeOfExpr _ _ (EBool _)                            = pure TBool
typeOfExpr ctx pos (EIdent ident)                   = typeOfIdent ctx pos ident
typeOfExpr ctx pos (EPlus e1 e2 )                   = typeOfBinop ctx pos e1 e2 TInt TInt
typeOfExpr ctx pos (ETimes e1 e2)                   = typeOfBinop ctx pos e1 e2 TInt TInt
typeOfExpr ctx pos (EMinus e1 e2)                   = typeOfBinop ctx pos e1 e2 TInt TInt
typeOfExpr ctx pos (ELess e1 e2)                    = typeOfBinop ctx pos e1 e2 TInt TBool
typeOfExpr ctx pos (EEqual e1 e2)                   = typeOfBinop ctx pos e1 e2 TInt TBool
typeOfExpr ctx pos (EApp e1 e2)                     = typeOfApp ctx pos e1 e2
typeOfExpr ctx pos (EIf e1 e2 e3)                   = typeOfIf ctx pos e1 e2 e3
typeOfExpr ctx pos (EFun ident param tparam tret e) = typeOfFuncDecl ctx pos ident param tparam tret e

typeOfApp :: Context -> Posn -> Expr -> Expr -> Either Error Ty
typeOfApp ctx pos e1 e2 = do
    tf <- typeOfExpr ctx pos e1
    targ <- typeOfExpr ctx pos e2

    case tf of
        TArrow from to ->
            if from == targ
                then pure to
                else Left (mkErr pos $ "Wrong type argument for function, wanted "
                                        <> show from <> ", but got " <> show targ)
        t              -> Left (mkErr pos $ show t <> " is not callable")


typeOfFuncDecl :: Context -> Posn -> String -> String -> Ty -> Ty -> Expr -> Either Error Ty
typeOfFuncDecl ctx pos ident param tparam tret e = do
    let ctx' = extendContext (extendContext ctx param tparam) ident (TArrow tparam tret)
    tret' <- typeOfExpr ctx' pos e
    if tret' /= tret
        then Left (mkErr pos $
            "Function return type is stated as " <> show tret
            <> ", but a " <> show tret' <> " was returned")
        else pure $ TArrow tparam tret

typeOfIf :: Context -> Posn -> Expr -> Expr -> Expr -> Either Error Ty
typeOfIf ctx pos condition thenBranch elseBranch = do
    ctype <- typeOfExpr ctx pos condition
    when (ctype /= TBool) $
        throwError (mkErr pos "Expected condition of if expression to be a boolean")

    t1 <- typeOfExpr ctx pos thenBranch
    t2 <- typeOfExpr ctx pos elseBranch
    when (t1 /= t2) $
        throwError (mkErr pos "Expected both branches of if expression to have the same type")

    pure t1

typeOfBinop :: Context -> Posn -> Expr -> Expr -> Ty -> Ty -> Either Error Ty
typeOfBinop ctx pos e1 e2 expectedType returnType = do
    t1 <- typeOfExpr ctx pos e1
    t2 <- typeOfExpr ctx pos e2

    if allSame [t1, t2, expectedType]
        then pure returnType
        else Left $ mkErr pos $ mconcat [ "Invalid types for binary operator: "
                                        , "expected ", show expectedType, " and ", show expectedType
                                        , ", but got ", show t1, " and ", show t2
                                        ]

typeOfIdent :: Context -> Posn -> String -> Either Error Ty
typeOfIdent ctx pos ident =
    maybe (Left $ mkErr pos $ "Unbound variable: " <> ident)
          pure
          (lookupContext ctx ident)

