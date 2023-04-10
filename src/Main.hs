module Main where

import           Control.Monad.IO.Class   (MonadIO)
import           System.Console.Haskeline

import           Parser                   (AnnotatedStmt (..), Stmt (..),
                                           parseTopLevel)
import           Typechecker              (Context, emptyContext, extendContext,
                                           typeOfExpr)

exec :: (MonadIO m) => Context -> String -> InputT m Context
exec ctx program =
    case parseTopLevel program of
        Left err                       -> outputStrLn err >> pure ctx
        Right (AStmt (SExpr expr) pos) -> do
            let ty = typeOfExpr ctx pos expr
            case ty of
                Left err  -> outputStrLn (show err) >> pure ctx
                Right ty' -> outputStrLn (show ty') >> pure ctx
        Right (AStmt (SLet ident expr) pos) -> do
            let ty = typeOfExpr ctx pos expr
            case ty of
                Left err -> outputStrLn (show err) >> pure ctx
                Right ty' -> do
                    outputStrLn $ show ty'
                    pure $ extendContext ctx ident ty'

repl :: IO ()
repl = do
    let loop ctx = do
            line <- getInputLine "> "
            case line of
                Nothing      -> pure ()
                Just ":quit" -> pure ()
                Just line'   -> exec ctx line' >>= loop
    runInputT defaultSettings (loop emptyContext)

main :: IO ()
main = repl
