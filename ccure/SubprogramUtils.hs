module SubprogramUtils where

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
