module Lib where

import Control.Applicative (Alternative(..))
import Data.Bifunctor (first)
import Data.Word (Word8)

newtype Parser a = Parser { parse :: String -> Maybe (a, String) }

char :: Char -> Parser Char
char c = Parser $ \s -> case s of
                          (c':cs) | c' == c -> Just (c, cs)
                          _ -> Nothing

instance Functor Parser where
  fmap f (Parser p) = Parser $ 
    \s -> fmap (first f) (p s)

instance Applicative Parser where
  pure a = Parser (\s -> Just (a, s))
  Parser p1 <*> Parser p2 = Parser $ \s -> do
    (f, str) <- p1 s
    (a, str') <- p2 str
    return (f a, str')

instance Monad Parser where
  return = pure
  Parser p >>= f = Parser $ \s -> 
    p s >>= (\(a, str) -> parse (f a) str)

instance Alternative Parser where
  empty =  Parser $ \s -> Nothing
  Parser p1 <|> Parser p2 = Parser $ \s -> p1 s <|> p2 s

string :: String -> Parser String
string "" = return ""
string (c:cs) = do
  char c
  string cs
  return $ c:cs

word8 :: Word8 -> Parser Word8
word8 = fmap read . string . show

data CoinageSymbol = Cu
                   | Ag
                   | Au
                   deriving (Show, Eq)

data CoinageMetal = CoinageMetal CoinageSymbol Word8 deriving (Show, Eq)

cu :: Parser CoinageMetal
cu = do
  string "Cu"
  isotope <- word8 63 <|> word8 65
  return $ CoinageMetal Cu isotope

ag :: Parser CoinageMetal
ag = do
  string "Ag"
  isotope <- word8 107 <|> word8 109
  return $ CoinageMetal Ag isotope

au :: Parser CoinageMetal
au = CoinageMetal Au <$> (string "Au" *> word8 197)

coinageMetal :: Parser CoinageMetal
coinageMetal = cu <|> ag <|> au

data ParseError = ParseFail | RemainingInput String deriving (Show, Eq)

runParser :: Parser a -> String -> Either ParseError a
runParser (Parser p) s = case p s of
  Nothing -> Left ParseFail
  Just (a, "") -> Right a
  Just (_, str) -> Left $ RemainingInput str
