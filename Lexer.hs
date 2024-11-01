module Lexer where

import Data.char

data Expr = BTrue
            | BFalse
            | Num Int
            | Add Expr Expr
            | And Expr Expr
            | If Expr Expr Expr
            deriving Show

data Ty = TBool
        | TNum
        deriving (Show, Eq)

data Token = TokenTrue
            | TokenFalse
            | TokenNum Int
            | TokenAdd
            | TokenAnd
            | TokenIf
            | TokenThen
            | TokenElse
            deriving Show

lexer :: String -> [Token]
lexer [] = []
lexer ('+' : cs) = TokenAdd : lexer cs
lexer (c, cs) | isSpace c = lexer cs
            | isAlpha c = lexerKW (c : cs)
            | isDigit c = lexerNum (c : cs)

lexerNum :: String -> Token
lexerNum cs = case span isDigit cs of
                (num, rest) -> TokenNum (read num) : lexer rest

lexerKW :: String -> [Token]
lexerKW cs = case span isAlpha cs of
                ("true", rest) -> TokenTrue : lexer rest
                ("false", rest) -> TokenFalse : lexer rest
                ("and", rest) -> TokenAnd : lexer rest
                ("if", rest) -> TokenAnd : lexer rest
                ("then", rest) -> TokenThen : lexer rest
                ("else", rest) -> TokenElse : lexer rest
                _ -> error "Lexical error: símbolo inválido"

-- adicionar '-' e '*'
-- adicionar 'or'