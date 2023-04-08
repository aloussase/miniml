module Main where

import           System.Console.Haskeline

import           Parser                   (parseTopLevel)

repl :: IO ()
repl = do
    let loop = do
            line <- getInputLine "> "
            case line of
                Nothing -> pure ()
                Just ":quit" -> pure ()
                Just line' -> do
                    let result = parseTopLevel line'
                    case result of
                        Left err  -> outputStrLn err
                        Right ast -> outputStrLn $ show ast
                    loop
    runInputT defaultSettings loop

main :: IO ()
main = repl
