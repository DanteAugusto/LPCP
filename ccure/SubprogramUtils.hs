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

insertUserFunction :: UserFunction -> CCureState -> CCureState
insertUserFunction f (a, b, c, d, e, g, funcList, h) = (a, b, c, d, e, g, f:funcList, h)

isInUserFunctions :: Token -> CCureState -> Bool
isInUserFunctions (Id id p) (_, _, _, _, _, _, funcList, _) = isInUserFunctionsAux (Id id p) funcList
isInUserFunctions _ _ = False

isInUserFunctionsAux :: Token -> [UserFunction] -> Bool
isInUserFunctionsAux _ [] = False
isInUserFunctionsAux (Id id p) ( (Id id1 p1, _, _, _):t ) = (id == id1) || isInUserFunctionsAux (Id id p) t
isInUserFunctionsAux _ _ = error "isInUserFunctionsAux: unexpected pattern"

getUserFunc :: Token -> CCureState -> UserFunction
getUserFunc (Id id p) (_, _, _, _, _, _, funcList, _) = getUserFuncAux (Id id p) funcList
getUserFunc _ _ = error "getUserFunc: unexpected pattern"

getUserFuncAux :: Token -> [UserFunction] -> UserFunction
getUserFuncAux _ [] = error "getUserFuncAux: function not found"
getUserFuncAux (Id id p) ( (Id id1 p1, pc, ret, param):t ) 
    = if id == id1 then (Id id1 p1, pc, ret, param) 
      else getUserFuncAux (Id id p) t
getUserFuncAux _ _ = error "getUserFuncAux: unexpected pattern"

compatibleArgs :: [Type] -> UserFunction -> Bool
compatibleArgs [] (_, _, _, []) = True
compatibleArgs [] _ = False
compatibleArgs _ (_, _, _, []) = False
compatibleArgs (a:as) (c, d, e, (_, b):bs) = (compatible (a, []) (b, [])) && compatibleArgs as (c, d, e, bs)

getBodyFromFunc :: UserFunction -> [Token]
getBodyFromFunc (_, pc, _, _) = pc
