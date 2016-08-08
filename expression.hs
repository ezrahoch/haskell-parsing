{-# OPTIONS -Wall -fwarn-incomplete-uni-patterns #-}
{-# LANGUAGE DeriveFunctor, LambdaCase #-}
import Control.Applicative
import Parser

main :: IO()
main = putStrLn "\\o/"

data Expression = Value Int
                | Sum [Expression]
                | Mul [Expression]
    deriving (Show)

parseExpressionVal :: Parser Expression
parseExpressionVal = Value <$> parseNum

parseExpressionSub :: Parser Expression
parseExpressionSub = parseSymbol '(' *> parseExpression <* parseSymbol ')'

sepBy :: Parser a -> Parser b -> Parser [a]
x `sepBy` sep = (:) <$> x <*> many (sep *> x)

parseInfix :: Char -> ([b] -> b) -> Parser b -> Parser b
parseInfix sym cons subExpr =
    ast <$> subExpr `sepBy` parseSymbol sym
    where
        ast [x] = x
        ast xs = cons xs

parseExpressionSum :: Parser Expression
parseExpressionSum = parseInfix '+' Sum parseExpressionMul

parseExpressionMul :: Parser Expression
parseExpressionMul = parseInfix '*' Mul parseExpressionTop

parseExpressionTop :: Parser Expression
parseExpressionTop = parseExpressionVal <|> parseExpressionSub

parseExpression :: Parser Expression
parseExpression = parseExpressionSum

computeExpression :: Expression -> Int
computeExpression =
    \case
        Value x      -> x
        Sum   (x:xs) -> computeExpression x + computeExpression (Sum xs)
        Mul   (x:xs) -> computeExpression x * computeExpression (Mul xs)
        Sum   []     -> 0
        Mul   []     -> 1

