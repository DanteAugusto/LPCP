{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Redundant return" #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# HLINT ignore "Use <$>" #-}
{-# HLINT ignore "Use bimap" #-}
module Compatibles where

import Lexer
import Text.Parsec
import State

compatible_op_unary :: Token -> (Type, [Token]) -> Bool
compatible_op_unary (Minus _) (IntType _, _) = True
compatible_op_unary (Minus _) (DoubleType _, _) = True
compatible_op_unary (Plus _) (IntType _, _) = True
compatible_op_unary (Plus _) (DoubleType _, _) = True
compatible_op_unary (Neg _) (BoolType _, _) = True
compatible_op_unary _ _ = False

compatible_op :: (Type, [Token]) -> Token -> (Type, [Token]) -> Bool
compatible_op (IntType _, _) (Plus _ ) (IntType _, _) = True
compatible_op (IntType _, _) (Minus _ ) (IntType _, _) = True
compatible_op (IntType _, _) (Mult _ ) (IntType _, _) = True
compatible_op (IntType _, _) (Divi _ ) (IntType _, _) = True
compatible_op (IntType _, _) (Mod _ ) (IntType _, _) = True
compatible_op (IntType _, _) (Expo _ ) (IntType _, _) = True
compatible_op (IntType _, _) (Lesser _ ) (IntType _, _) = True
compatible_op (IntType _, _) (Greater _ ) (IntType _, _) = True
compatible_op (IntType _, _) (LessEq _ ) (IntType _, _) = True
compatible_op (IntType _, _) (GreatEq _ ) (IntType _, _) = True
compatible_op (IntType _, _) (Eq _ ) (IntType _, _) = True
compatible_op (IntType _, _) (Diff _ ) (IntType _, _) = True

compatible_op (DoubleType _, _) (Plus _ ) (DoubleType _, _) = True
compatible_op (DoubleType _, _) (Minus _ ) (DoubleType _, _) = True
compatible_op (DoubleType _, _) (Mult _ ) (DoubleType _, _) = True
compatible_op (DoubleType _, _) (Divi _ ) (DoubleType _, _) = True
compatible_op (DoubleType _, _) (Expo _ ) (DoubleType _, _) = True
compatible_op (DoubleType _, _) (Lesser _ ) (DoubleType _, _) = True
compatible_op (DoubleType _, _) (Greater _ ) (DoubleType _, _) = True
compatible_op (DoubleType _, _) (LessEq _ ) (DoubleType _, _) = True
compatible_op (DoubleType _, _) (GreatEq _ ) (DoubleType _, _) = True
compatible_op (DoubleType _, _) (Eq _ ) (DoubleType _, _) = True
compatible_op (DoubleType _, _) (Diff _ ) (DoubleType _, _) = True

compatible_op (BoolType _, _) (And _ ) (BoolType _, _) = True
compatible_op (BoolType _, _) (Or _ ) (BoolType _, _) = True
compatible_op (BoolType _, _) (Eq _ ) (BoolType _, _) = True
compatible_op (BoolType _, _) (Diff _ ) (BoolType _, _) = True

compatible_op (MatrixInt (l1, c1, _), _) (PlusMatrix _) (MatrixInt (l2, c2, _), _) = l1 == l2 && c1 == c2
compatible_op (MatrixInt (l1, c1, _), _) (MultMatrix _) (MatrixInt (l2, c2, _), _) = c1 == l2

compatible_op (MatrixDouble (l1, c1, _), _) (PlusMatrix _) (MatrixDouble (l2, c2, _), _) = l1 == l2 && c1 == c2
compatible_op (MatrixDouble (l1, c1, _), _) (MultMatrix _) (MatrixDouble (l2, c2, _), _) = c1 == l2

compatible_op (IntType _, _) (Mult _) (MatrixInt _, _) = True
compatible_op (MatrixInt _, _) (Mult _) (IntType _, _) = True

compatible_op (DoubleType _, _) (Mult _) (MatrixDouble _, _) = True
compatible_op (MatrixDouble _, _) (Mult _) (DoubleType _, _) = True

compatible_op (IntType _, _) (Plus _) (MatrixInt _, _) = True
compatible_op (MatrixInt _, _) (Plus _) (IntType _, _) = True

compatible_op (DoubleType _, _) (Plus _) (MatrixDouble _, _) = True
compatible_op (MatrixDouble _, _) (Plus _) (DoubleType _, _) = True

compatible_op _ _ _ = False

compatible :: (Type, [Token]) -> (Type, [Token]) -> Bool
compatible (IntType _, _) (IntType _, _) = True
compatible (DoubleType _, _) (DoubleType _, _) = True
compatible (BoolType _, _) (BoolType _, _) = True
compatible (StringType _, _) (StringType _, _) = True
compatible (RegisterType (a, _), _) (RegisterType (b, _), _) = a == b
compatible _ _ = False

compatible_matrix :: (Type, [Token]) -> (Type, [Token]) -> Bool
compatible_matrix (MatrixInt _, _) (IntType _, _) = True
compatible_matrix (MatrixDouble _, _) (DoubleType _, _) = True
compatible_matrix _ _ = False

compatible_varDecl :: Token -> (Type, [Token]) -> Bool
compatible_varDecl (Int _) (IntType _, _) = True
compatible_varDecl (Double _) (DoubleType _, _) = True
compatible_varDecl (Bool _) (BoolType _, _) = True
compatible_varDecl (Str _) (StringType _, _) = True
compatible_varDecl _ _ = False

compatible_matrix_assign :: Token -> (Type, [Token]) -> Type -> Type -> Bool
compatible_matrix_assign (Int _) (IntType _, _) _ _ = True
compatible_matrix_assign (Double _) (DoubleType _, _) _ _= True

compatible_matrix_assign (Int _) (MatrixInt (l1, c1, _), _) (IntType l2) (IntType c2) = l1 == l2 && c1 == c2
compatible_matrix_assign (Double _) (MatrixDouble (l1, c1, _), _) (IntType l2) (IntType c2) = l1 == l2 && c1 == c2

compatible_matrix_assign _ _ _ _ = False
