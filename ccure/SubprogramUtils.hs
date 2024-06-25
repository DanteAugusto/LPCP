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

compatibleArgs :: [Type] -> UserFunction -> Bool
compatibleArgs [] (_, _, _, []) = True
compatibleArgs [] _ = False
compatibleArgs _ (_, _, _, []) = False
compatibleArgs (a:as) (c, d, e, (_, b):bs) = (compatible (a, []) (b, [])) && compatibleArgs as (c, d, e, bs)

compatibleArgsP :: [(Token, String, Int, Type)] -> UserProc -> Bool
compatibleArgsP [] (_, _, []) = True
compatibleArgsP [] _ = False
compatibleArgsP _ (_, _, []) = False
compatibleArgsP ((Id "$notref" _, _, _, a):as) (c, d, (_, b, True):bs) = False
compatibleArgsP ((Id "$notref" _, _, _, a):as) (c, d, (_, b, False):bs) = (compatible (a, []) (b, [])) && compatibleArgsP as (c, d, bs)
compatibleArgsP ((_, _, _, a):as)         (c, d, (_, b, False):bs) = False
compatibleArgsP ((_, _, _, a):as)         (c, d, (_, b, True):bs) = (compatible (a, []) (b, [])) && compatibleArgsP as (c, d, bs)

getBodyFromFunc :: UserFunction -> [Token]
getBodyFromFunc (_, pc, _, _) = pc

getBodyFromProc :: UserProc -> [Token]
getBodyFromProc (_, pc, _) = pc

