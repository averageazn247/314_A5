import System.Environment
import Data.List
import Data.Char
import Data.Maybe

main = do
  [f] <- getArgs
  s   <- readFile f
  writeFile "output.txt" (concat (map (\x -> (token_to_string x) ++ " ") (lexer s)))

data AST = AST Token AST AST | FLOAT Float | STRING String

data Token = LPAREN | RPAREN | EQUAL | SEMICOLON | ADDITION | SUBTRACTION | MULTIPLICATION | DIVISION | FP Float | VARIABLE String
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
  | head input == '+' = [ADDITION] ++ lexer (tail input)
  | head input == '-' = [SUBTRACTION] ++ lexer (tail input)
  | head input == '*' = [MULTIPLICATION] ++ lexer (tail input)
  | head input == '/' = [DIVISION] ++ lexer (tail input)
  | isAlpha (head input) = [VARIABLE (take (fromJust (findIndex (\x -> not (isAlphaNum x)) (input))) (input))] ++ lexer (drop (fromJust (findIndex (\x -> not (isAlphaNum x)) (tail input))) (tail input))
  | isDigit (head input) = [FP 0] ++ lexer (tail input)-- take in a floating point
  | otherwise = [FP 1] ++ lexer (tail input)

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

parse_expression :: [Token] -> (AST, [Token])
                         -- first token after parsing term == +   =  tree with + as the root, left side is parse_term, right side is the parse_expression of the leftover token from parsing the left side
parse_expression input | head (snd (parse_term input)) == ADDITION = (AST ADDITION (fst (parse_term input)) (fst (parse_expression (tail (snd (parse_term input))))), snd (parse_expression (tail (snd (parse_term input)))))

parse_term :: [Token] -> (AST, [Token])
parse_term input = parse_primary input

parse_primary :: [Token] -> (AST, [Token])
parse_primary input = parse_fp input
 
parse_fp :: [Token] -> (AST, [Token])
parse_fp (FP x:xs) = (FLOAT x, xs)