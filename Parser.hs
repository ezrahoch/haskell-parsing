{-# OPTIONS -Wall -fwarn-incomplete-uni-patterns #-}
{-# LANGUAGE DeriveFunctor, LambdaCase #-}

module Parser where

import qualified Data.Char as Char
import Data.Maybe (listToMaybe)
import Data.Function ((&))
import Control.Applicative

second :: (b -> b') -> (a, b) -> (a, b')
second f (x, y) = (x, f y)

newtype Parser a = Parser { runParser :: String -> Either String (String, a) }
    deriving (Functor)

instance Applicative Parser where
    pure x = Parser $ \s -> Right (s, x)
    Parser pf <*> Parser px =
        Parser $
        \s ->
            do
                (s', f) <- pf s
                (fmap . second) f (px s')

parserFail :: String -> Parser a
parserFail err = Parser $ \_ -> Left err

instance Monad Parser where
    return = pure
    fail = parserFail
    Parser x >>= f =
        Parser $
        \s -> do
            (s', a) <- x s
            runParser (f a) s'

instance Alternative Parser where
    empty = parserFail "empty"
    Parser px <|> Parser py =
        Parser $
        \s -> case px s of
            Left _ -> py s
            Right r -> Right r
    some v = (:) <$> v <*> many v
    many v = some v <|> pure []

parseChar :: Parser Char
parseChar = Parser $ \case
    (c:cs) -> Right (cs, c)
    [] -> Left "parseChar encountered eof"

peekString :: Parser String
peekString = Parser $ \s -> Right (s, s)

--

peekChar :: Parser (Maybe Char)
peekChar = listToMaybe <$> peekString

satisfy :: (Char -> Bool) -> Parser Char
satisfy p =
    Parser $
    \case
    [] -> Left "satisfy hit eof"
    (c:cs)
        | p c -> Right (cs, c)
        | otherwise -> Left "satisfy didn't pass predicate"


parseDigit :: Parser Int
parseDigit = Char.digitToInt <$> satisfy Char.isDigit


-- Parses digits into num
parseNum :: Parser Int
parseNum = combineDigits <$> some parseDigit
    where
        combineDigits ds = zipWith (*) (reverse ds) (iterate (*10) 1) & sum

some_ :: Alternative f => f a -> f ()
some_ p = p *> many_ p

many_ :: Alternative f => f a -> f ()
many_ p = (() <$ p) <|> pure ()

skipWhitespace :: Parser ()
skipWhitespace = () <$ satisfy Char.isSpace

skipWhitespaces :: Parser ()
skipWhitespaces = many_ skipWhitespace

parseSymbol :: Char -> Parser Char
parseSymbol c = satisfy (== c)
