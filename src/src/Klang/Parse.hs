{-# LANGUAGE OverloadedStrings #-}

module Klang.Parse where
import Prelude hiding (takeWhile)
import Data.Attoparsec.Text
import Data.Char
import qualified Data.Text as Tx
import Control.Monad
import Control.Applicative
import Klang.Type
import Klang.Config

data SExp = Lit Data | Var String | Func String [SExp]
    deriving Eq

instance Show SExp where
    show (Lit d) = "Lit[" ++ show d ++ "]"
    show (Var s) = "Var[" ++ s ++ "]"
    show (Func s xs) = "(" ++ s ++ "\n" ++ unlines ( map ("  " ++) $ lines $ unlines $ show <$> xs) ++ ")"

reserved :: [Tx.Text]
reserved = map Tx.pack ((fst <$> kFunc1) ++ (fst <$> kFunc2) ++ ["let", "lambda"])

skipSpace1 :: Parser ()
skipSpace1 = void $ takeWhile1 isSpace

sExpHead :: Parser a -> Parser a
sExpHead p = (char '(' *> skipSpace *> p) <* skipSpace1

sExpTail :: Parser [SExp]
sExpTail = pure <$> choice (map (\p -> p <* skipSpace <* char ')') sExps) <|> do
    h <- choice $ map (<* skipSpace1) sExps
    rest <- sExpTail
    return (h : rest)

atoms :: [Parser SExp]
atoms = [
    string "True" >> return (Lit (KBool True)),
    string "False" >> return (Lit (KBool False)),
    string "nil" >> return (Lit KNil),
    (Lit . KDouble) <$> double,
    do
        _ <- char '\''
        c <- (char '\\' >> char '\'') <|> (char '\\' >> char '\\') <|> satisfy (/='\'')
        _ <- char '\''
        return (Lit (KChar c)),
    do
        _ <- char '"'
        s <- many' $ (char '\\' >> char '"') <|> (char '\\' >> char '\\') <|> satisfy (/='"')
        _ <- char '"'
        return (Lit $ foldr (curry KPair) KNil (KChar <$> s)),
    do
        h <- satisfy $ (||) <$> isAlpha <*> (=='_')
        t <- takeWhile $ (||) <$> isAlphaNum <*> (=='_')
        return (Var $ h : Tx.unpack t)
    ]

func :: Tx.Text -> Parser SExp
func s = do
    s <- sExpHead $ string s
    t <- sExpTail
    return $ Func (Tx.unpack s) t

primitives :: [Parser SExp]
primitives = map func reserved

applyFunc :: Parser SExp
applyFunc = do
    s <- choice $ map sExpHead sExps
    t <- sExpTail
    return $ Func "apply" (s : t)

sExps :: [Parser SExp]
sExps = atoms ++ primitives ++ [applyFunc]

parseProg :: Tx.Text -> Either String SExp
parseProg = parseOnly $ do
    skipSpace
    prog <- choice sExps
    skipSpace
    endOfInput
    return prog

parseManyProg :: Tx.Text -> Either String [SExp]
parseManyProg = parseOnly $ do
    skipSpace
    progs <- choice sExps `sepBy1` skipSpace1
    skipSpace
    endOfInput
    return progs
