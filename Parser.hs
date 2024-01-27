module Parser (parse_expr, parse_code) where

import Control.Monad
import Control.Applicative
import Expr
import Data.Char

-- Parser data type
newtype Parser a = Parser {
    parse :: String -> Maybe(a, String)
}

--- type declaration ---

instance Monad Parser where
    return x = Parser $ \s -> Just (x, s)
    mp >>= f = Parser $ \s -> case parse mp s of
                                        Nothing -> Nothing
                                        Just (x, s') -> parse (f x) s'

instance Applicative Parser where
    pure x = return x
    pf <*> px = do
        f <- pf
        x <- px
        return $ f x

instance Functor Parser where
    fmap f px = do
        x <- px
        return $ f x

instance Alternative Parser where
    empty = failParser
    p1 <|> p2 = Parser $
        \s -> case parse p1 s of
            Nothing -> parse p2 s
            ok -> ok

--- type declaration over ---

failParser :: Parser a
failParser = Parser $ \s -> Nothing

charParser :: Char -> Parser Char
charParser c = Parser $
                    \s -> case s of
                        [] -> Nothing
                        (x : xs) -> if x == c then Just (x, xs) else Nothing

predicateParser :: (Char -> Bool) -> Parser Char
predicateParser p = Parser $
                        \s -> case s of
                            [] -> Nothing
                            (x : xs) -> if p x then Just (x, xs) else Nothing

plusParser :: Parser a -> Parser [a]
plusParser p =
    do
        x <- p
        xs <- starParser p
        return (x : xs)

starParser :: Parser a -> Parser [a]
starParser p = plusParser p <|> return []

-- parses a variable, which can only have alphabetical caracters
varParser :: Parser String
varParser =
    do
        x <- predicateParser isAlpha
        xs <- starParser $ predicateParser isAlpha
        return (x : xs)

-- parses a string, which can only begin with an alphabetical char, others can be alphanumerical
stringParser :: Parser String
stringParser =
    do
        x <- predicateParser isAlpha
        xs <- starParser $ predicateParser isAlphaNum
        return (x : xs)

whiteSpaceParser :: Parser String
whiteSpaceParser = starParser $ charParser ' '

-- parse a expression
varExprParser :: Parser Expr
varExprParser = Variable <$> varParser

functionExprParser :: Parser Expr
functionExprParser =
    do
        charParser '\\'
        x <- stringParser
        charParser '.'
        e <- atomExprParser
        return $ Function x e

initPlusAtomParser :: Parser a -> Parser [a]
initPlusAtomParser p =
    do
        x <- p
        xs <- starAtomParser p
        return (x : xs)

continuePlusAtomParser :: Parser a -> Parser [a]
continuePlusAtomParser p =
    do
        charParser ' '
        x <- p
        xs <- starAtomParser p
        return (x : xs)

starAtomParser :: Parser a -> Parser [a]
starAtomParser p = continuePlusAtomParser p <|> return []

applicationExprParser :: Parser Expr
applicationExprParser =
    do
        exprs <- initPlusAtomParser atomExprParser
        return $ foldl Application (head exprs) (tail exprs)

parenthesesExprParser :: Parser Expr
parenthesesExprParser =
    do
        charParser '('
        e <- exprParser
        charParser ')'
        return e

macroParser :: Parser Expr
macroParser =
    do
        charParser '$'
        m <- stringParser
        return $ Macro m

atomExprParser :: Parser Expr
atomExprParser = macroParser <|> varExprParser <|> functionExprParser <|> parenthesesExprParser

exprParser :: Parser Expr
exprParser = applicationExprParser <|> atomExprParser

parse_expr :: String -> Expr
parse_expr s = case parse exprParser s of
                        Just (e, "") -> e
                        Just (e, notParsed) -> error ("Parsing failed! Parsed: " ++ show e ++ "; Not parsed: " ++ notParsed)
                        Nothing -> error "Error while parsing!"

-- parse code
assignParser :: Parser Code
assignParser =
    do
        m <- stringParser
        whiteSpaceParser
        charParser '='
        whiteSpaceParser
        e <- exprParser
        return $ Assign m e

evaluateParser :: Parser Code
evaluateParser =
    do
        e <- exprParser
        return $ Evaluate e

codeParser :: Parser Code
codeParser = assignParser <|> evaluateParser

parse_code :: String -> Code
parse_code s = case parse codeParser s of
                        Just (e, "") -> e
                        Just (e, notParsed) -> error ("Parsing failed! Parsed: " ++ show e ++ "; Not parsed: " ++ notParsed)
                        Nothing -> error "Error while parsing!"