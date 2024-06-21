module MatrixUtils where

import Lexer
import Text.Parsec
import State
import Tokens (intLitToken)

validDimensions :: Token -> Token -> Bool
validDimensions (IntLit v1 _) (IntLit v2 _) = v1 > 0 && v2 > 0
validDimensions _ _ = error "Dimesions must be int literals"


makeMatrix :: Num a => Int -> Int -> a -> [[a]]
makeMatrix l c d = replicate l (replicate c d)

makeMatrixType :: Token -> Token -> Token -> Type
makeMatrixType (IntLit l _) (IntLit c _) (IntLit v _) = MatrixInt (l, c, makeMatrix l c v)
makeMatrixType (IntLit l _) (IntLit c _) (DoubleLit v _) = MatrixDouble (l, c, makeMatrix l c v)
makeMatrixType _ _ _ = error "Invalid Params given to makeMatrixType"