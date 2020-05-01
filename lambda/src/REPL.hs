module REPL (hmRepl, hmReplVerbose) where

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

hmReplVerbose :: IO ()
hmReplVerbose = putStrLn "" >> loop where
    loop = do
        -- print $ instantiate (Forall 0 $ Forall 1 $ Mono $ pairT (TVar 0) (TVar 1) :-> TVar 0) 0
        putStr " \955> "
        hFlush stdout
        line <- getLine
        case parse line of
            Just expr -> do
                let res = genConstraints expr ctx
                case res of
                    Right (t,c) -> do
                        putStrLn $ "Type: " ++ show t
                        sequence_ $ print <$> c
                        putStrLn ""
                        case unify c of
                            Right sub -> do
                                sequence_ $ print <$> sub
                                putStrLn $ displayExpr expr ++ " : " ++ (show $ generalize $ applySubstitution sub t)
                            Left err -> putStrLn $ "Error: " ++ show err
                    Left err -> putStrLn $ "Error: " ++ show err
            Nothing -> printErr "Could not parse input."
        loop

intT = TApp "int" []
pairT a b = TApp "pair" [a,b]

ctx :: Context
ctx = [
    ("0", Mono $ intT),
    ("1", Mono $ intT),
    ("2", Mono $ intT),
    ("3", Mono $ intT),
    ("4", Mono $ intT),
    ("5", Mono $ intT),
    ("add", Mono $ intT :-> intT :-> intT),
    ("sub", Mono $ intT :-> intT :-> intT),
    ("pair", Forall 0 $ Forall 1 $ Mono $ TVar 0 :-> TVar 1 :-> pairT (TVar 0) (TVar 1)),
    ("fst", Forall 0 $ Forall 1 $ Mono $ pairT (TVar 0) (TVar 1) :-> TVar 0),
    ("snd", Forall 0 $ Forall 1 $ Mono $ pairT (TVar 0) (TVar 1) :-> TVar 1),
    ("id", Forall 0 $ Mono $ TVar 0 :-> TVar 0)]

printErr :: String -> IO ()
printErr e = do
    putStr "Error: "
    putStrLn e
