
module ErrorMessages where
import Lexer
import State
import Conversions

alreadyDeclaredError :: String -> String
alreadyDeclaredError var = "Variable \"" ++ var ++ "\" already declared. Remember: for your CCurence, we do not support shadowing!"

notDeclaredError :: String -> String
notDeclaredError var = "Variable \"" ++ var ++ "\" not declared."

typeErrorMessage :: Token -> Type -> String
typeErrorMessage expectedType receivedType = "Type error: expected " ++ show expectedType ++ " but received " ++ show (typeToToken receivedType) ++ "."

typeErrorReturn :: Token -> Type -> String
typeErrorReturn expectedType receivedType = "Type error: expected " ++ show expectedType ++ " but received " ++ show (typeToToken receivedType) ++ "."

typeErrorInput :: Token -> String -> String
typeErrorInput id input = "Type error: couldn't convert input \""++ input ++"\" to be of type " ++ show id ++ "."

invalidProcedureCall :: Token -> String
invalidProcedureCall id = "Invalid procedure call: " ++ show id ++ " not declared."

invalidFunctionCall :: Token -> String
invalidFunctionCall id = "Invalid function call: " ++ show id ++ " not declared."

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

typeErrorDefaultRegister :: Token -> Token -> String
typeErrorDefaultRegister expectedType receivedType = "Type error on Default constructor: expected " ++ show expectedType ++ " but received " ++ show receivedType ++ "."

invalidUserType :: Token -> String
invalidUserType id = "Invalid user type: " ++ show id ++ " not declared."

invalidUserTypeReturn :: Token -> String
invalidUserTypeReturn id = "Invalid user type in return: " ++ show id ++ " not declared."

invalidUserTypeParameter :: Token -> String
invalidUserTypeParameter id = "Invalid user type in parameter: " ++ show id ++ " not declared."

invalidRegisterAccess :: Token -> Token -> String
invalidRegisterAccess id a = "Invalid register access: " ++ show id ++ " does not have attribute " ++ show a ++ "."

isNotRegisterType :: Token -> String
isNotRegisterType id = "Invalid register access: " ++ show id ++ " is not a register type."

matrixPositiveIntegersAccess :: String
matrixPositiveIntegersAccess = "Acessing a matrix requires positive integers as indexes."

matrixPositiveIntegersDimensions :: String
matrixPositiveIntegersDimensions = "Creating a matrix requires positive integers as dimensions."

matrixIncompatibleValueAssigned :: String
matrixIncompatibleValueAssigned = "Incompatible value assigned to matrix. It needs to be a matrix with same lines, collumns and type of the matrix, or a scalar value of the same type of the matrix."


matrixAccessOutOfBounds :: String
matrixAccessOutOfBounds = "Acessing a matrix out of bounds."

-- algo = (Plus _) <|> (Minus _) <|> ...

invalidOperation :: Token -> Token -> Token -> String
invalidOperation a (Plus p) b = "Invalid operation: " ++ show (Plus p) ++ " " ++ " received incompatible types " ++ show a ++ " and " ++ show b ++ "."
invalidOperation a (Minus p) b = "Invalid operation: " ++ show (Minus p) ++ " " ++ " required two operands of the same type (Int or Double), but received " ++ show a ++ " and " ++ show b ++ "."
invalidOperation a (Mult p) b = "Invalid operation: " ++ show (Mult p) ++ " " ++ " received incompatible types " ++ show a ++ " and " ++ show b ++ "."
invalidOperation a (Divi p) b = "Invalid operation: " ++ show (Divi p) ++ " " ++ " required two operands of the same type (Int or Double), but received " ++ show a ++ " and " ++ show b ++ "."
invalidOperation a (Mod p) b = "Invalid operation: " ++ show (Mod p) ++ " " ++ " required two operands of the same type (Int), but received " ++ show a ++ " and " ++ show b ++ "."
invalidOperation a (Expo p) b = "Invalid operation: " ++ show (Expo p) ++ " " ++ " required two operands of the same type (Int or Double), but received " ++ show a ++ " and " ++ show b ++ "."
invalidOperation a (Lesser p) b = "Invalid operation: " ++ show (Lesser p) ++ " " ++ " required two operands of the same type (Int or Double), but received " ++ show a ++ " and " ++ show b ++ "."
invalidOperation a (Greater p) b = "Invalid operation: " ++ show (Greater p) ++ " " ++ " required two operands of the same type (Int or Double), but received " ++ show a ++ " and " ++ show b ++ "."
invalidOperation a (LessEq p) b = "Invalid operation: " ++ show (LessEq p) ++ " " ++ " required two operands of the same type (Int or Double), but received " ++ show a ++ " and " ++ show b ++ "."
invalidOperation a (GreatEq p) b = "Invalid operation: " ++ show (GreatEq p) ++ " " ++ " required two operands of the same type (Int or Double), but received " ++ show a ++ " and " ++ show b ++ "."
invalidOperation a (Eq p) b = "Invalid operation: " ++ show (Eq p) ++ " " ++ " required two operands of the same type (Int, Double or Bool), but received " ++ show a ++ " and " ++ show b ++ "."
invalidOperation a (Diff p) b = "Invalid operation: " ++ show (Diff p) ++ " " ++ " required two operands of the same type (Int, Double or Bool), but received " ++ show a ++ " and " ++ show b ++ "."
invalidOperation a (And p) b = "Invalid operation: " ++ show (And p) ++ " " ++ " required two operands of the same type (Bool), but received " ++ show a ++ " and " ++ show b ++ "."
invalidOperation a (Or p) b = "Invalid operation: " ++ show (Or p) ++ " " ++ " required two operands of the same type (Bool), but received " ++ show a ++ " and " ++ show b ++ "."
invalidOperation a (PlusMatrix p) b = "Invalid operation: " ++ show (PlusMatrix p) ++ " " ++ " required two matrixes of equal dimensions and of the same type (IntMatrix or DoubleMatrix)."
invalidOperation a (MultMatrix p) b = "Invalid operation: " ++ show (MultMatrix p) ++ " " ++ " required two matrixes of compatible dimensions (collums of first equal to lines of second) and of the same type (IntMatrix or DoubleMatrix)."
invalidOperation _ _ _ = "Invalid operation: unknown error."

invalidUnaryOperation :: Token -> Token -> String
invalidUnaryOperation (Plus p) a = "Invalid unary operation: " ++ show (Plus p) ++ " " ++ " required an operand of type Int or Double, but received " ++ show a ++ "."
invalidUnaryOperation (Minus p) a = "Invalid unary operation: " ++ show (Minus p) ++ " " ++ " required an operand of type Int or Double, but received " ++ show a ++ "."
invalidUnaryOperation (Neg p) a = "Invalid unary operation: " ++ show (Neg p) ++ " " ++ " required an operand of type Bool, but received " ++ show a ++ "."
invalidUnaryOperation _ _ = "Invalid unary operation: unknown error."


controlNotBoolError :: Token -> String
controlNotBoolError id = "Control structure error: " ++ show id ++ " is not of type Bool."

breakOutOfLoopError :: String
breakOutOfLoopError = "Break statement outside of loop."

returnOutOfFunctionError :: String
returnOutOfFunctionError = "Return statement outside of function."


castError :: Token -> Token -> String
castError a b = "Cast error: cannot cast " ++ show a ++ " to " ++ show b ++ ". Only widening casts are allowed."