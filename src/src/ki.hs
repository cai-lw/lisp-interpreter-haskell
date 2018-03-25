{-# LANGUAGE FlexibleInstances, FlexibleContexts, TupleSections, OverloadedStrings #-}
import Klang.Parse
import Klang.Compile
import Klang.Type hiding (unpack)
import Data.Char
import Data.List
import Data.Maybe
import System.Environment
import System.Console.GetOpt
import System.IO
import Control.Monad
import Control.Monad.Except
import Data.Attoparsec.Text
import qualified Data.Text as Tx
import qualified Data.Text.IO as Tx

interpret :: Tx.Text -> Res
interpret s = parseProg s >>= interpretSExp

interpretSExp :: SExp -> Res
interpretSExp s = compile s >>= (\k -> run k [])

showE :: (Show a) => Either String a -> String
showE (Left e) = "Error: " ++ e
showE (Right o) = show o

loop :: (Monad m) => a -> (a -> m a) -> m b
loop x f = do
    y <- f x
    loop y f

parseRepl :: Parser (Char, Tx.Text)
parseRepl = do
    _ <- char ':'
    c <- anyChar
    skipSpace1
    rest <- takeText
    return (c, rest)

repl :: IO ()
repl = do
    putStrLn "Ki <Liwei Cai> @ REPL mode"
    void $ runExceptT $ loop "" $ \prev -> do
        lift $ putStr ">>> "
        lift $ hFlush stdout
        s <- lift Tx.getLine
        case parseOnly parseRepl s of
            Right (com, input) -> case com of
                'i' -> do
                    let res = interpret input
                    lift $ putStrLn $ showE res
                    case res of
                        Right _ -> return input
                        Left  _ -> return prev
                't' -> do
                    unless (Tx.null input) $ lift $ putStrLn "Warning: :t accepts no argument. Displaying AST of previous program."
                    unless (Tx.null prev) $ lift $ putStrLn $ showE $ parseProg prev
                    return prev
                'q' -> do
                    unless (Tx.null input) $ lift $ putStrLn "Warning: :q accepts no argument. Leaving anyway."
                    lift $ putStrLn "Leaving Ki."
                    exit
                c   -> do
                    lift $ putStrLn $ "Unknown command \'" ++ [c] ++ "\'"
                    return prev
            Left _ -> do
                lift $ putStrLn "Wrong command syntax"
                return prev

optionDef :: [OptDescr (Char, String)]
optionDef = [
    Option ['h'] ["help"] (NoArg ('h', ""))      "Display help",
    Option []    ["repl"] (NoArg ('r', ""))      "Run in REPL mode",
    Option ['i'] []       (ReqArg ('i',) "FILE") "Interpret code in FILE",
    Option ['t'] []       (ReqArg ('t',) "FILE") "Print AST of code in FILE",
    Option ['o'] []       (ReqArg ('o',) "FILE") "Output to FILE. Default is stdout"
    ]

exit :: (MonadError () m) => m a
exit = throwError ()

main :: IO ()
main = void $ runExceptT $ do
    (options, args, errors) <- getOpt Permute optionDef <$> lift getArgs
    unless (null errors) $ do
        lift $ mapM_ putStrLn errors
        lift $ putStrLn "type \"ki --help\" to see usage."
        exit
    unless (null args) $ do
        lift $ putStrLn "Doesn't accept non-option arguments."
        lift $ putStrLn "type \"ki --help\" to see usage."
        exit

    let isHelp = isJust $ find ((=='h') . fst) options
    when isHelp $ lift (putStrLn $ usageInfo "Ki <Liwei Cai>" optionDef) >> exit

    let isRepl = isJust $ find ((=='r') . fst) options
    when isRepl $ lift repl >> exit

    inputFile <- case find (((||) <$> (=='i') <*> (=='t')) . fst) options of
        Just (_, s) | not (null s) -> return s
        _ -> do
            lift $ putStrLn "No input file specified"
            exit
    outputFile <- case find ((=='o') . fst) options of
        Just (_, s) | not (null s) -> return s
        _ -> return ""
    outputHandle <- if null outputFile then return stdout else lift $ openFile outputFile WriteMode
    let isInterpret = isJust $ find ((=='i') . fst) options
    code <- lift $ Tx.readFile inputFile
    progs <- case parseManyProg code of
        Right ps -> return ps
        e -> do
            lift $ putStrLn $ showE e
            exit
    let outputs = [if isInterpret then showE $ interpretSExp prog else show prog | prog <- progs]
    lift $ mapM_ (hPutStrLn outputHandle) outputs
