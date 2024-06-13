
type ExAct = Bool

-- Nome, Pilha (profundidade de chamada, valor)
type SymTable = [(String, [(Int, Token)])]
type ExecStack = [[]]
type ActRegFunc = [(String, [String])]
type ActRegProc = [[]]

type State = (SymTable, ExecStack, ActRegFunc, ActRegProc, ExAct)
