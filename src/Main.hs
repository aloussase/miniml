module Main where

import           Control.Monad.IO.Class   (MonadIO)
import           System.Console.Haskeline

import           Compiler
import           Machine
import           Parser                   (AnnotatedStmt (..), Stmt (..),
                                           parseTopLevel)
import           Typechecker              (Context, emptyContext, extendContext,
                                           typeOfExpr)

exec :: (MonadIO m) => (Context, Environ) -> String -> InputT m (Context, Environ)
exec state@(ctx, env) program =
    case parseTopLevel program of
        Left err                       -> outputStrLn err >> pure state
        Right (AStmt (SExpr expr) pos) -> do
            let ty = typeOfExpr ctx pos expr
            case ty of
                Left err  -> outputStrLn (show err) >> pure state
                Right ty' -> do
                    let instructions = compile expr
                    let value = run instructions env
                    outputStrLn $ "- : " <> show ty' <> " = " <> show value
                    pure state
        Right (AStmt (SLet ident expr) pos) -> do
            let ty = typeOfExpr ctx pos expr
            case ty of
                Left err -> outputStrLn (show err) >> pure state
                Right ty' -> do
                    let instructions = compile expr
                    let value = run instructions env
                    outputStrLn $ ident <> ": " <> show ty' <> " = " <> show value
                    pure (extendContext ctx ident ty', (ident,value):env)

repl :: IO ()
repl = do
    let loop (ctx, env) = do
            line <- getInputLine "> "
            case line of
                Nothing      -> pure ()
                Just ":quit" -> pure ()
                Just line'   -> exec (ctx, env) line' >>= loop
    runInputT defaultSettings (loop (emptyContext, []))

main :: IO ()
main = repl
