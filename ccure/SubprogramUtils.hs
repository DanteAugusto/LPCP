{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
module SubprogramUtils where

import Compatibles
import Lexer
import Text.Parsec
import State

-- type UserProcedures = [(Token, [Token], [(Token, Type)])]
-- type UserFunctions = [(Token, [Token], Type, [(Token, Type)])]

-- insertUserProcedure :: Token -> CCureState -> CCureState
-- insertUserProcedure (Id id p) (a, b, c, d, e, f, g, h) = (a, b, c, d, e, (Id id p, [], []):f, g, h)

type UserFunction = (Token, [Token], Type, [(Token, Type)])

type UserProc = (Token, [Token], [(Token, Type, Bool)])

insertUserFunction :: UserFunction -> CCureState -> CCureState
insertUserFunction f (a, b, c, d, e, g, funcList, h) = (a, b, c, d, e, g, f:funcList, h)

insertUserProc :: UserProc -> CCureState -> CCureState
insertUserProc f (a, b, c, d, e, procList, funcList, h) = (a, b, c, d, e, f:procList, funcList, h)

isInUserFunctions :: Token -> CCureState -> Bool
isInUserFunctions (Id id p) (_, _, _, _, _, _, funcList, _) = isInUserFunctionsAux (Id id p) funcList
isInUserFunctions _ _ = False

isInUserProcs :: Token -> CCureState -> Bool
isInUserProcs (Id id p) (_, _, _, _, _, procList, _, _) = isInUserProcsAux (Id id p) procList
isInUserProcs _ _ = False

isInUserFunctionsAux :: Token -> [UserFunction] -> Bool
isInUserFunctionsAux _ [] = False
isInUserFunctionsAux (Id id p) ( (Id id1 p1, _, _, _):t ) = (id == id1) || isInUserFunctionsAux (Id id p) t
isInUserFunctionsAux _ _ = error "isInUserFunctionsAux: unexpected pattern"

isInUserProcsAux :: Token -> [UserProc] -> Bool
isInUserProcsAux _ [] = False
isInUserProcsAux (Id id p) ( (Id id1 p1, _, _):t ) = (id == id1) || isInUserProcsAux (Id id p) t
isInUserProcsAux _ _ = error "isInUserProcsAux: unexpected pattern"

getUserFunc :: Token -> CCureState -> UserFunction
getUserFunc (Id id p) (_, _, _, _, _, _, funcList, _) = getUserFuncAux (Id id p) funcList
getUserFunc _ _ = error "getUserFunc: unexpected pattern"

getUserProc :: Token -> CCureState -> UserProc
getUserProc (Id id p) (_, _, _, _, _, proccList, _, _) = getUserProcAux (Id id p) proccList
getUserProc _ _ = error "getUserProc: unexpected pattern"

getUserFuncAux :: Token -> [UserFunction] -> UserFunction
getUserFuncAux _ [] = error "getUserFuncAux: function not found"
getUserFuncAux (Id id p) ( (Id id1 p1, pc, ret, param):t ) 
    = if id == id1 then (Id id1 p1, pc, ret, param) 
      else getUserFuncAux (Id id p) t
getUserFuncAux _ _ = error "getUserFuncAux: unexpected pattern"

getUserProcAux :: Token -> [UserProc] -> UserProc
getUserProcAux _ [] = error "getUserProcAux: function not found"
getUserProcAux (Id id p) ( (Id id1 p1, pc, param):t ) 
    = if id == id1 then (Id id1 p1, pc, param) 
      else getUserProcAux (Id id p) t
getUserProcAux _ _ = error "getUserProcAux: unexpected pattern"

-- Tipos de erros
-- 0 - Sem erro
-- 1 - Número de argumentos menor ou maior que o esperado
-- 4 - Argumento incompatível

compatibleArgs :: Int -> [Type] -> UserFunction -> (Int, Int, Type, Type) -- (Erro, NumParametro, Tipo Real, Tipo Formal)
compatibleArgs _ [] (_, _, _, []) = (0, 0, NULL, NULL)
compatibleArgs i [] _ = (1, i, NULL, NULL)
compatibleArgs i _ (_, _, _, []) = (1, i, NULL, NULL)
compatibleArgs i (a:as) (c, d, e, (_, b):bs) = 
  if( compatible (a, []) (b, []) ) then compatibleArgs (i + 1) as (c, d, e, bs)
  else (4, i, a, b)

-- Tipos de erros
-- 0 - Sem erro
-- 1 - Número de argumentos menor que o esperado
-- 1 - Número de argumentos maior que o esperado
-- 2 - Argumento nao passado por referencia, mas esperado ser passado por referencia
-- 3 - Argumento passado por referencia, mas esperado nao ser passado por referencia
-- 4 - Argumento incompatível

compatibleArgsP :: Int -> [(Token, String, Int, Type)] -> UserProc -> (Int, Int, Type, Type) -- (Erro, NumParametro, Tipo Real, Tipo Formal)
compatibleArgsP _ [] (_, _, []) = (0, 0, NULL, NULL)
compatibleArgsP i [] _ = (1, i, NULL, NULL)
compatibleArgsP i _ (_, _, []) = (1, i, NULL, NULL)
compatibleArgsP i ((Id "$notref" _, _, _, a):as) (c, d, (_, b, True):bs) = (2, i, NULL, NULL)
compatibleArgsP i ((Id "$notref" _, _, _, a):as) (c, d, (_, b, False):bs) = 
  if( compatible (a, []) (b, []) ) then compatibleArgsP (i + 1) as (c, d, bs)
  else (4, i, a, b)
compatibleArgsP i ((_, _, _, a):as)         (c, d, (_, b, False):bs) = (3, i, NULL, NULL)
compatibleArgsP i ((_, _, _, a):as)         (c, d, (_, b, True):bs) = 
  if( compatible (a, []) (b, []) ) then compatibleArgsP (i + 1) as (c, d, bs)
  else (4, i, a, b)

getBodyFromFunc :: UserFunction -> [Token]
getBodyFromFunc (_, pc, _, _) = pc

getBodyFromProc :: UserProc -> [Token]
getBodyFromProc (_, pc, _) = pc

