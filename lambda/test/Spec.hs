import qualified Parsers.LambdaParser as LP

main :: IO ()
main = parseTest

parseTest :: IO ()
parseTest = do
    print ""
    print $ LP.parse "\\n.\\f.\\x.f (n f x)"
    print $ LP.parse "\\n f x. f (n f x)"
    print $ LP.parse "\\  n f x . f((n f)x)"