module State where

import Lexer
import Text.Parsec

data LoopStatus = OK | BREAK | CONTINUE deriving (Eq, Show)
type ExAct = Bool
-- Nome, profundidade de chamada, valor idea do grande amigo fouquet
type SymTable = [(String, [(Int, Token)])]
type SymTableErrada = [(Token, Token)]


-- type ExecStack = [[]] pilha de instancias de ra (linha pra onde devo voltar e valor de retorno)
-- type ActRegSubprog = [(String, [String])]
type ScopeStack = [String]
type LoopStack = [LoopStatus]

type CCureState = (SymTableErrada, ScopeStack, LoopStack, ExAct)

-- funções auxiliares para o CCureState

addToLoopStack :: CCureState -> LoopStatus -> CCureState
addToLoopStack (a, b,stack, c) newLoopStatus = (a, b, newLoopStatus:stack, c)

removeFromLoopStack :: CCureState -> CCureState
removeFromLoopStack (_, _, [], _) = error "trying to remove unexistent Loop"
removeFromLoopStack (a, b, stack:tail, c) = (a, b, tail, c)

getCurrentLoopStatus :: CCureState -> LoopStatus
getCurrentLoopStatus (_, _, top:tail, _) = top
getCurrentLoopStatus _ = error "trying to access unexistent LoopStatus"



addToScopeStack :: CCureState -> String -> CCureState
addToScopeStack (a, stack, b, c) newScope = (a, newScope:stack, b, c)
-- addToScopeStack (_, top:tail, _) newScope = (_, (newScope ++ ['#'] ++ top):top:tail, _)

getCurrentScope :: CCureState -> String
getCurrentScope (_, top:tail, _, _) = top
getCurrentScope _ = error "trying to access unexistent scope"

removeFromScopeStack :: CCureState -> CCureState
removeFromScopeStack (_, [], _, _) = error "trying to remove unexistent scope"
removeFromScopeStack (a, stack:tail, b, c) = (a, tail, b, c)

-- getCurrentScope :: CCureState -> String
-- getCurrentScope (_, [top]:tail, _) = getCurrentScopeAux(top)

-- getCurrentScopeAux :: String -> String
-- getCurrentScopeAux [] = ""
-- getCurrentScopeAux '#':tail = ""
-- getCurrentScopeAux top:tail = top : getCurrentScopeAux tail

execOn :: CCureState -> Bool
execOn (_, _, _, True) = True
execOn (_, _, _, False) = False

turnExecOn :: CCureState -> CCureState
turnExecOn (a, b, c, _) = (a, b, c, True)

turnExecOff :: CCureState -> CCureState
turnExecOff (a, b, c, _) = (a, b, c, False)