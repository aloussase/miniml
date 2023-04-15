module Main where

import           Control.Monad.State
import qualified Data.ByteString.Char8 as B8
import           Network.Simple.TCP

import           Compiler
import           Machine
import           Parser                (AnnotatedStmt (..), Stmt (..),
                                        parseTopLevel)
import           Typechecker           (Context, emptyContext, extendContext,
                                        typeOfExpr)

data AppState = AppState
    { appStateContext :: !Context
    , appStateEnviron :: !Environ
    }

initialState :: AppState
initialState = AppState emptyContext []

type AppM = StateT AppState IO

exec :: String -> AppM String
exec program = do
    ctx <- gets appStateContext
    env <- gets appStateEnviron

    case parseTopLevel program of
        Left err                       -> pure $ show err
        Right (AStmt (SExpr expr) pos) -> do
            let ty = typeOfExpr ctx pos expr
            case ty of
                Left err  -> pure $ show err
                Right ty' -> do
                    let instructions = compile expr
                    let value = run instructions env
                    let result = "- : " <> show ty' <> " = " <> show value
                    pure result
        Right (AStmt (SLet ident expr) pos) -> do
            let ty = typeOfExpr ctx pos expr
            case ty of
                Left err -> pure $ show err
                Right ty' -> do
                    let instructions = compile expr
                    let value = run instructions env
                    let result = ident <> ": " <> show ty' <> " = " <> show value

                    put $ AppState
                        { appStateContext = extendContext ctx ident ty'
                        , appStateEnviron = (ident,value) : env
                        }

                    pure result

loop :: Socket -> AppM ()
loop clientSocket = do
    line <- liftIO $ recv clientSocket 4096
    case B8.unpack <$> line of
        Nothing    -> pure ()
        Just line' -> do
            result <- exec line'
            liftIO $ putStrLn result
            liftIO $ send clientSocket (B8.pack result)
            loop clientSocket

main :: IO ()
main = serve (Host "127.0.0.1") "3693" $ \(clientSocket, _) ->
        evalStateT (loop clientSocket) initialState
