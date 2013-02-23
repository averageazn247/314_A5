import Lexer

parse_expression :: [Token] -> (AST, [Token])
parse_expression input | head result_leftovers == ADDITION = (NODE ADDITION result_AST (fst (parse_expression (tail result_leftovers))), snd (parse_expression (tail result_leftovers)))
  where term_result = parse_term input
        result_AST = fst term_result
        result_leftovers = snd term_result

parse_term :: [Token] -> (AST, [Token])
parse_term input = parse_primary input

parse_primary :: [Token] -> (AST, [Token])
parse_primary input = parse_fp input
 
parse_fp :: [Token] -> (AST, [Token])
parse_fp (FP x:xs) = (FLOAT x, xs)
