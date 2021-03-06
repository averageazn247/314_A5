import Lexer

import System.Environment
import Data.List
import Data.Char
import Data.Maybe
import Data.Map

main :: IO ()
main = do
  [f] <- getArgs
  expression <- readFile f
  read_eval_print expression 1 (Data.Map.empty)

read_eval_print :: String -> Int -> (Map String AST) -> IO ()
read_eval_print expression count variable_map =
  if (eol) == Nothing then return ()
                      else if (fromJust eol) == 0 then read_eval_print (tail expression) count variable_map
  else if (head (lexer thisline) == VARIABLE "var") then read_eval_print nextline (count+1) (parse_variable (tail (lexer thisline)) (variable_map))
  else if (head (lexer thisline) == VARIABLE "def") then read_eval_print nextline (count+1) (parse_method (tail (lexer thisline)) (variable_map))
  else do
  writeFile ("output" ++ (show count) ++ ".txt") (show (evaluate (transformAST(fst (parse_expression (lexer (thisline))))) variable_map) )
  read_eval_print (nextline) (count + 1) variable_map
  where
    eol = (elemIndex '\n' expression)
    nextline = drop (fromJust eol) expression
    thisline = take (fromJust eol) expression
{-
does a tree transformation to take care of the associativity problem
the initial parsing makes everything right associative, which is invalid for subtraction and division
this does a rotation as shown below, with a similar transform for division

               -                                                     +
  leftAST               +               ----->             -                 rightAST
              innerAST     rightAST               leftAST    innerAST
-}
transformAST :: AST -> AST
transformAST (NODE SUBTRACTION left (NODE ADDITION inner right)) = NODE ADDITION (NODE SUBTRACTION (transformAST left) (transformAST inner)) (transformAST right)
transformAST (NODE DIVISION left (NODE MULTIPLICATION inner right)) = NODE MULTIPLICATION (NODE DIVISION (transformAST left) (transformAST inner)) (transformAST right)
transformAST (NODE ADDITION y z) = NODE ADDITION (transformAST y) (transformAST z)
transformAST (NODE MULTIPLICATION y z) = NODE MULTIPLICATION (transformAST y) (transformAST z)
transformAST (NODE SUBTRACTION y z) = NODE SUBTRACTION (transformAST y) (transformAST z)
transformAST (NODE DIVISION y z) = NODE DIVISION (transformAST y) (transformAST z)
transformAST (x) =  x

parse_method :: [Token] -> Map String AST -> Map String AST
parse_method (VARIABLE name:LPAREN:xs) var_map = Data.Map.insertWithKey collision_handler name value var_map
  where rhs = transformAST (fst (parse_expression (drop ((fromJust (elemIndex EQUAL xs))+1) xs)))
        arguments = extract_arguments ((take ((parse_parentheses xs 1 0)-1) xs) ++ [COMMA])
        value = replace_args rhs arguments 

replace_args :: AST -> [String] -> AST
replace_args (VAR x) args
  | index /= Nothing = ARG_NUM ((fromJust index)+1)
  | otherwise = VAR x
  where index = (elemIndex x args) 
replace_args (NODE t lhs rhs) args = (NODE t (replace_args lhs args) (replace_args rhs args))
replace_args tree args = tree

extract_arguments :: [Token] -> [String]
extract_arguments [] = []
extract_arguments (VARIABLE var:COMMA:xs) = var:(extract_arguments xs)

parse_variable :: [Token] -> Map String AST -> Map String AST
parse_variable (VARIABLE name:xs) var_map = Data.Map.insertWithKey collision_handler name new_ast var_map
  where new_ast = transformAST (fst (parse_expression (drop 1 xs)))

collision_handler :: String -> AST -> AST -> AST
collision_handler key new_value old_value = new_value

parse_expression :: [Token] -> (AST, [Token])
parse_expression input 
  | input == [] = error "Expected: expression, Actual: end of input"
  |      result_leftovers == [] = parse_term input
  | head result_leftovers == LPAREN = error "Expected: expression operator, Actual: left parentheses"
  | head result_leftovers == RPAREN = error "Expected: expression operator, Actual: right parentheses"
  | head result_leftovers == ADDITION = (NODE ADDITION result_AST (fst (parse_expression (tail result_leftovers))), snd (parse_expression (tail result_leftovers)))
  | head result_leftovers == SUBTRACTION = (NODE SUBTRACTION result_AST (fst (parse_expression (tail result_leftovers))), snd (parse_expression (tail result_leftovers)))
  | otherwise = parse_term input
  where term_result = parse_term input
        result_AST = fst term_result
        result_leftovers = snd term_result

parse_term :: [Token] -> (AST, [Token])
parse_term input
  | input == [] = error "Expected: term, Actual: end of input"
  |      result_leftovers == [] = parse_primary input
  | head result_leftovers == LPAREN = error "Expected: term operator, Actual: left parentheses"
  | head result_leftovers == RPAREN = error "Expected: term operator, Actual: right parentheses"
  | head result_leftovers == MULTIPLICATION = (NODE MULTIPLICATION result_AST (fst (parse_term (tail result_leftovers))), snd (parse_term (tail result_leftovers)))
  | head result_leftovers == DIVISION = (NODE DIVISION result_AST (fst (parse_term (tail result_leftovers))), snd (parse_term (tail result_leftovers)))
  | otherwise = parse_primary input
  where primary_result = parse_primary input
        result_AST = fst primary_result
        result_leftovers = snd primary_result

parse_primary :: [Token] -> (AST, [Token])
parse_primary [] = error "Expected: primary, Actual: end of input"
parse_primary (LPAREN:xs) = (fst (parse_expression (take ((parse_parentheses xs 1 0)-1) xs)), drop ((parse_parentheses xs 1 0)) xs)
parse_primary (FP x:xs) = (FLOAT x, xs)
parse_primary (VARIABLE x:LPAREN:xs) = ((METHOD x (parse_arguments ((take ((parse_parentheses xs 1 0)-1) xs) ++ [COMMA]))), (drop ((parse_parentheses xs 1 0)) xs))
parse_primary (VARIABLE x:xs) = (VAR x, xs)
parse_primary (x:xs) = error ("Expected: primary, Actual: " ++ token_to_string x)

parse_arguments :: [Token] -> [AST]
parse_arguments input  
  | input == [] = []
  | head input == COMMA = parse_arguments (tail input)
  | otherwise = (transformAST (fst (parse_expression (take index  input)))):(parse_arguments (drop (index+1) input))
  where index = fromJust (elemIndex COMMA input)

-- returns an int specifying the number of elements in the input that are part of the top parentheses level
parse_parentheses :: [Token] -> Int -> Int -> Int
parse_parentheses input level count
  | level == 0 = count
  | input == [] = error "Expected: right parentheses, Actual: end of input"
  | head input == LPAREN = parse_parentheses (tail input) (level + 1) (count + 1)
  | head input == RPAREN = parse_parentheses (tail input) (level - 1) (count + 1)
  | otherwise = parse_parentheses (tail input) level (count + 1)

evaluate :: AST -> Map String AST -> Float
evaluate (FLOAT x )                 var_map  = x
evaluate (NODE ADDITION l r )       var_map  = (evaluate l var_map ) + (evaluate r var_map )
evaluate (NODE SUBTRACTION l r )    var_map  = (evaluate l var_map ) - (evaluate r var_map )
evaluate (NODE MULTIPLICATION l r ) var_map  = (evaluate l var_map ) * (evaluate r var_map )
evaluate (NODE DIVISION l r )       var_map  = (evaluate l var_map ) / (evaluate r var_map )
evaluate (VAR v )                   var_map  = evaluate (fromJust (Data.Map.lookup v var_map)) var_map
evaluate (METHOD m args)            var_map  = evaluate (match_args ( fromJust (Data.Map.lookup m var_map)) args ) var_map

match_args :: AST -> [AST] -> AST
match_args (ARG_NUM n) args = args!!(n-1)
match_args (NODE op l r) args = NODE op (match_args l args) (match_args r args)
match_args x y = x












