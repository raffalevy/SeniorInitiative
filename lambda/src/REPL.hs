module REPL (hmRepl) where

import System.IO

import HindleyMilner
import Parsers.HMParser

hmRepl :: IO ()
hmRepl = putStrLn "" >> loop where
    loop = do
        putStr " \955> "
        hFlush stdout
        line <- getLine
        case parse line of
            Just expr -> case infer expr ctx of
                Right t -> do
                    putStr "Type: "
                    putStrLn $ show t
                Left e -> printErr $ show e
            Nothing -> printErr "Could not parse input."
        loop

intT = TApp "int" []

ctx :: Context
ctx = [
    ("0", Mono $ intT),
    ("1", Mono $ intT),
    ("2", Mono $ intT),
    ("3", Mono $ intT),
    ("add", Mono $ intT :-> intT :-> intT),
    ("id", Forall 0 $ Mono $ TVar 0 :-> TVar 0)]

printErr :: String -> IO ()
printErr e = do
    putStr "Error: "
    putStrLn e