module MatrixUtils where

import Lexer
import Text.Parsec
import State
import Tokens (intLitToken)
import Data.List

validDimensions :: Type -> Type -> Bool
validDimensions (IntType v1) (IntType v2) = v1 > 0 && v2 > 0
validDimensions _ _ = False

makeMatrix :: Num a => Int -> Int -> a -> [[a]]
makeMatrix l c d = replicate l (replicate c d)

makeMatrixType :: Type -> Type -> Type -> Type
makeMatrixType (IntType l) (IntType c) (IntType v) = MatrixInt (l, c, makeMatrix l c v)
makeMatrixType (IntType l) (IntType c) (DoubleType v) = MatrixDouble (l, c, makeMatrix l c v)

makeMatrixType _ _ (MatrixInt v) = MatrixInt v
makeMatrixType _ _ (MatrixDouble v) = MatrixDouble v


makeMatrixType _ _ _ = error "Invalid Params given to makeMatrixType"

sumMatrix :: Num a => [[a]] -> [[a]] -> [[a]]
sumMatrix = zipWith (zipWith (+))

mulMatrix :: Num a => [[a]] -> [[a]] -> [[a]]
mulMatrix x y = [[sum $ zipWith (*) xs ys | ys <- transpose y] | xs <- x]

scalarMul :: Num a => a -> [[a]] -> [[a]]
scalarMul s = (map . map) (*s)

scalarSum :: Num a => a -> [[a]] -> [[a]]
scalarSum s = (map . map) (+s)