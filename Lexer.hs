module Lexer where

import System.Environment
import Data.List
import Data.Char
import Data.Maybe

data AST = NODE Token AST AST | FLOAT Float | VAR String | METHOD String [AST] | ARG_NUM Int | EMPTY

printAST :: AST -> String -> String
printAST (NODE token lChild rChild) prefix = "\n\t" ++ prefix  ++ printAST lChild (prefix ++ "\t") ++ "\n" ++ prefix ++ token_to_string token ++ "\n\t" ++ prefix ++ printAST rChild (prefix ++ "\t")
printAST (FLOAT float) input = show float
printAST (VAR string) input = show string
printAST EMPTY input = ""

data Token = LPAREN | RPAREN | EQUAL | SEMICOLON | COMMA | ADDITION | SUBTRACTION | MULTIPLICATION | DIVISION | FP Float | VARIABLE String
  deriving Eq
--type FP = Float
--type VARIABLE = String

lexer :: [Char] -> [Token]
lexer input
  |      input == []  = []
  | head input == ' ' = lexer (tail input)
  | head input == '\n'= lexer (tail input)
  | head input == '(' = [LPAREN] ++ lexer (tail input)
  | head input == ')' = [RPAREN] ++ lexer (tail input)
  | head input == '=' = [EQUAL]  ++ lexer (tail input)
  | head input == ';' = [SEMICOLON] ++ lexer (tail input)
  | head input == ',' = [COMMA] ++ lexer (tail input)
  | head input == '+' = [ADDITION] ++ lexer (tail input)
  | head input == '-' = [SUBTRACTION] ++ lexer (tail input)
  | head input == '*' = [MULTIPLICATION] ++ lexer (tail input)
  | head input == '/' = [DIVISION] ++ lexer (tail input)
  | endString == Nothing && isAlpha (head input) = [VARIABLE input]
  | isAlpha (head input) = [VARIABLE (take (fromJust endString) (input))] ++ lexer (drop (fromJust endString) (input))
  | endFloat == Nothing && isDigit (head input)  = [FP (read input)]
  | isDigit (head input) = [FP (read (take (fromJust endFloat) (input)) :: Float)] ++ lexer (drop (fromJust endFloat) (input))
  | otherwise = [FP 1] ++ lexer (tail input)
  where 
  endFloat = findIndex (\x -> not ((isDigit x) || x == '.')) (input)
  endString = findIndex (\x -> not (isAlphaNum x)) (input)

token_to_string :: Token -> [Char]
token_to_string input
  | input == LPAREN = "LPAREN"
  | input == RPAREN = "RPAREN"
  | input == EQUAL = "EQUAL"
  | input == SEMICOLON = "SEMICOLON"
  | input == ADDITION = "ADDITION"
  | input == SUBTRACTION = "SUBTRACTION"
  | input == MULTIPLICATION = "MULTIPLICATION"
  | input == DIVISION = "DIVISION"
token_to_string t@(FP x) = show x
token_to_string t@(VARIABLE x) = show x

