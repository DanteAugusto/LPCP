module State where

import Lexer
import Text.Parsec

type ExAct = Bool
-- Nome, profundidade de chamada, valor
type SymTable = [(String, [(Int, Token)])]
-- type ExecStack = [[]] pilha de instancias de ra (linha pra onde devo voltar e valor de retorno)
type ActRegSubprog = [(String, [String])]
type ScopeStack = [String]

type State = (SymTable, ActRegSubprog, ScopeStack, UserTypes, ExAct)
