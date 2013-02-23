import Lexer

import System.Environment
import Data.List
import Data.Char
import Data.Maybe

main = do
  [f] <- getArgs
  s   <- readFile f
  writeFile "output.txt" (printAST (fst (parse_expression (lexer s))))

parse_expression :: [Token] -> (AST, [Token])
parse_expression input 
  | input == [] = (EMPTY, [])
  | head input == LPAREN = parse_expression (tail input)
  |      result_leftovers == [] = parse_term input
  | head result_leftovers == RPAREN = (fst (parse_primary [head input]), tail result_leftovers)
  | head result_leftovers == ADDITION = (NODE ADDITION result_AST (fst (parse_expression (tail result_leftovers))), snd (parse_expression (tail result_leftovers)))
  | head result_leftovers == SUBTRACTION = (NODE SUBTRACTION result_AST (fst (parse_expression (tail result_leftovers))), snd (parse_expression (tail result_leftovers)))
  | otherwise = parse_term input
  where term_result = parse_term input
        result_AST = fst term_result
        result_leftovers = snd term_result

parse_term :: [Token] -> (AST, [Token])
parse_term input
  | input == [] = (EMPTY, [])
  | head input == LPAREN = parse_expression (tail input)
  |      result_leftovers == [] = parse_primary input
  | head result_leftovers == RPAREN = (fst (parse_primary [head input]), tail result_leftovers)
  | head result_leftovers == MULTIPLICATION = (NODE MULTIPLICATION result_AST (fst (parse_term (tail result_leftovers))), snd (parse_term (tail result_leftovers)))
  | head result_leftovers == DIVISION = (NODE DIVISION result_AST (fst (parse_term (tail result_leftovers))), snd (parse_term (tail result_leftovers)))
  | otherwise = parse_primary input
  where primary_result = parse_primary input
        result_AST = fst primary_result
        result_leftovers = snd primary_result

parse_primary :: [Token] -> (AST, [Token])
parse_primary input
  | input == [] = (EMPTY, [])
parse_primary (FP x:xs) = (FLOAT x, xs)
parse_primary (VARIABLE x:xs) = (VAR x, xs)
