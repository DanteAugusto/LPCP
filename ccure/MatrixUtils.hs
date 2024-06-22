module MatrixUtils where

import Lexer
import Text.Parsec
import State
import Tokens (intLitToken)
import Data.List

validDimensions :: Type -> Type -> Bool
validDimensions (IntType v1) (IntType v2) = v1 > 0 && v2 > 0
validDimensions _ _ = False

validAccess :: Type -> Type -> Bool
validAccess (IntType v1) (IntType v2) = v1 >= 0 && v2 >= 0
validAccess _ _ = False
 

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

canAccesMatrix :: Type -> Type -> Type -> Bool
canAccesMatrix (IntType l) (IntType c) (MatrixInt (lm, cm, _)) = l < lm && c < cm
canAccesMatrix (IntType l) (IntType c) (MatrixDouble (lm, cm, _)) = l < lm && c < cm
canAccesMatrix _ _ _ = False

getLitFromMatrix :: Num a => Int -> Int -> [[a]] -> a
getLitFromMatrix l c m = (m!!l)!!c 

getValFromMatrix :: Type -> Type -> Type -> Type
getValFromMatrix (IntType l) (IntType c) (MatrixInt (lm, cm, m)) = IntType $ getLitFromMatrix l c m
getValFromMatrix (IntType l) (IntType c) (MatrixDouble (lm, cm, m)) = DoubleType $ getLitFromMatrix l c m
getValFromMatrix _ _ _ = error "Cannot get val from matrix"

changeCol :: Num a => Int -> [a] -> a -> [a]
changeCol 0 (x:xs) v = v:xs
changeCol c (x:xs) v = x:changeCol (c-1) xs v
changeCol _ _ _ = error "Putz"

changeMatrix :: Num a => Int -> Int -> [[a]] -> a -> [[a]]
changeMatrix 0 c (xs:xss) v = changeCol c xs v:xss
changeMatrix l c (xs:xss) v = xs:changeMatrix (l-1) c xss v
changeMatrix _ _ _ _ = error "Putz 2"