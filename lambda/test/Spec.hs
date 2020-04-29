{-# LANGUAGE PatternSynonyms #-}

import qualified Parsers.LambdaParser as LP
import qualified HindleyMilner as HM
import HindleyMilner (pattern (:->), pattern (:$))

main :: IO ()
main = putStrLn "" >> parseTest

parseTest :: IO ()
parseTest = do
    print ""
    print $ LP.parse "\\n.\\f.\\x.f (n f x)"
    print $ LP.parse "\\n f x. f (n f x)"
    print $ LP.parse "\\  n f x . f((n f)x)"

zeroHM :: HM.Expr
zeroHM = HM.Lam "f" $ HM.Lam "x" $ HM.Var "x"

pattern V x = HM.Var x

f = V "f"
n = V "n"
x = V "x"

succHM :: HM.Expr
succHM = HM.Lam "n" $ HM.Lam "f" $ HM.Lam "x" $ f :$ (n :$ f :$ x)
intHM :: HM.Type
intHM = HM.Mono $ HM.TApp "int" []

applyTo3HM :: HM.Expr
applyTo3HM = HM.Lam "f" $ HM.App (HM.Var "f") (HM.Var "3")

hmTest :: IO ()
hmTest = succHMTest
-- hmTest = print $ HM.infer (succHM :$ V "z") [("z", intHM)]

hmTestOmega = hmTestExpr (HM.Lam "x" $ x :$ x) []

-- forall 4 5 2. ((4 -> 5) -> (2 -> 4)) -> ((4 -> 5) -> (2 -> 5))

applyTo3Test = hmTestExpr applyTo3HM [("3", intHM)]

hmTestExpr :: HM.Expr -> HM.Context -> IO ()
hmTestExpr expr ctx = do
    let res = HM.genConstraints expr ctx
    case res of
        Right (t,c) -> do
            putStrLn $ "Type: " ++ show t
            sequence_ $ print <$> c
            putStrLn ""
            case HM.unify c of
                Right sub -> do
                    sequence_ $ print <$> sub
                    putStrLn $ HM.displayExpr expr ++ " : " ++ (show $ HM.generalize $ HM.applySubstitution sub t)
                Left err -> putStrLn $ "Error: " ++ show err
        Left err -> putStrLn $ "Error: " ++ show err

succHMTest :: IO ()
succHMTest = hmTestExpr succHM []