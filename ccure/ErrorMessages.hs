
module ErrorMessages where
import Lexer
import State
import Conversions

alreadyDeclaredError :: String -> String
alreadyDeclaredError var = "Variable \"" ++ var ++ "\" already declared. Remember: for your CCurence, we do not support shadowing!"

typeErrorMessage :: Token -> Type -> String
typeErrorMessage expectedType receivedType = "Type error: expected " ++ show expectedType ++ " but received " ++ show (typeToToken receivedType) ++ "."

invalidProcedureCall :: Token -> String
invalidProcedureCall id = "Invalid procedure call: " ++ show id ++ " not declared."

-- Tipos de erros
-- 0 - Sem erro
-- 1 - Número de argumentos menor que o esperado
-- 2 - Argumento nao passado por referencia, mas esperado ser passado por referencia
-- 3 - Argumento passado por referencia, mas esperado nao ser passado por referencia
-- 4 - Argumento incompatível
-- (Erro, NumParametro, Tipo Real, Tipo Formal) -> TamanhoListaReal -> TamanhoListaFormal
invalidArgsProcedure :: (Int, Int, Type, Type) -> Int -> Int -> String
invalidArgsProcedure (1, _, _, _) i j  = "Invalid number of arguments: expected " ++ show j ++ " but received " ++ show i ++ "."
invalidArgsProcedure (2, i, _, _) _ _ = "Argument " ++ show i ++ " expected to be passed by reference."
invalidArgsProcedure (3, i, _, _) _ _ = "Argument " ++ show i ++ " expected to be passed by value."
invalidArgsProcedure (4, i, a, b) _ _ = "Argument " ++ show i ++ " expected to be of type " ++ show (typeToToken b) ++ " but received " ++ show (typeToToken a) ++ "."