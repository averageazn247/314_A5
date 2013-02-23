import Lexer

import System.Environment
import Data.List
import Data.Char
import Data.Maybe

main = do
  [f] <- getArgs
  s   <- readFile f
  writeFile "output.txt" (printAST (transformAST(fst (parse_expression (lexer s)))) "")

transformAST :: AST -> AST
transformAST (NODE SUBTRACTION left (NODE ADDITION inner right)) = NODE ADDITION (NODE SUBTRACTION (transformAST left) (transformAST inner)) (transformAST right)
transformAST (NODE DIVISION left (NODE MULTIPLICATION inner right)) = NODE MULTIPLICATION (NODE DIVISION (transformAST left) (transformAST inner)) (transformAST right)
transformAST (NODE ADDITION y z) = NODE ADDITION (transformAST y) (transformAST z)
transformAST (NODE MULTIPLICATION y z) = NODE MULTIPLICATION (transformAST y) (transformAST z)
transformAST (NODE SUBTRACTION y z) = NODE SUBTRACTION (transformAST y) (transformAST z)
transformAST (NODE DIVISION y z) = NODE DIVISION (transformAST y) (transformAST z)
transformAST (FLOAT x) = FLOAT x
transformAST (VAR x) = VAR x
transformAST EMPTY = EMPTY

parse_expression :: [Token] -> (AST, [Token])
parse_expression input 
  | input == [] = (EMPTY, [])
  |      result_leftovers == [] = parse_term input
  | head result_leftovers == ADDITION = (NODE ADDITION result_AST (fst (parse_expression (tail result_leftovers))), snd (parse_expression (tail result_leftovers)))
  | head result_leftovers == SUBTRACTION = (NODE SUBTRACTION result_AST (fst (parse_expression (tail result_leftovers))), snd (parse_expression (tail result_leftovers)))
  | otherwise = parse_term input
  where term_result = parse_term input
        result_AST = fst term_result
        result_leftovers = snd term_result

parse_term :: [Token] -> (AST, [Token])
parse_term input
  | input == [] = (EMPTY, [])
  |      result_leftovers == [] = parse_primary input
  | head result_leftovers == MULTIPLICATION = (NODE MULTIPLICATION result_AST (fst (parse_term (tail result_leftovers))), snd (parse_term (tail result_leftovers)))
  | head result_leftovers == DIVISION = (NODE DIVISION result_AST (fst (parse_term (tail result_leftovers))), snd (parse_term (tail result_leftovers)))
  | otherwise = parse_primary input
  where primary_result = parse_primary input
        result_AST = fst primary_result
        result_leftovers = snd primary_result

parse_parentheses :: [Token] -> Int -> Int -> Int
parse_parentheses input level count
  | level == 0 = count
  | head input == LPAREN = parse_parentheses (tail input) (level + 1) (count + 1)
  | head input == RPAREN = parse_parentheses (tail input) (level - 1) (count + 1)
  | otherwise = parse_parentheses (tail input) level (count + 1)

parse_primary :: [Token] -> (AST, [Token])
parse_primary [] = (EMPTY, [])
parse_primary (LPAREN:xs) = (fst (parse_expression (take ((parse_parentheses xs 1 0)-1) xs)), drop ((parse_parentheses xs 1 0)) xs)
parse_primary (FP x:xs) = (FLOAT x, xs)
parse_primary (VARIABLE x:xs) = (VAR x, xs)
