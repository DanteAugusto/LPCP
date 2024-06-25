module Conversions where

import Lexer
import State

typeToToken :: Type -> Token
typeToToken (IntType _) = Int (0, 0)
typeToToken (DoubleType _) = Double (0, 0)
typeToToken (BoolType _) = Bool (0, 0)
typeToToken (StringType _) = Str (0, 0)
typeToToken (RegisterType (TypeId id p, l)) = TypeId id (0, 0)
typeToToken (MatrixInt (l, c, m)) = Matrix (0, 0)
typeToToken (MatrixDouble (l, c, m)) = Matrix (0, 0)

