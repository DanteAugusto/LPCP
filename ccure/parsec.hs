{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Redundant return" #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# HLINT ignore "Use <$>" #-}
{-# HLINT ignore "Use bimap" #-}
{-# HLINT ignore "Redundant if" #-}
module Main (main) where

import Compatibles
import Conversions
import Lexer
import State
import Tokens
import MatrixUtils
import RegisterUtils
import SubprogramUtils
import Text.Read
import Text.Parsec
import Control.Monad.IO.Class
import System.IO.Unsafe
import Text.Read.Lex
import Data.Maybe
import Distribution.Compat.Lens (_1)
import System.IO
import System.Environment
import Distribution.Compat.Directory (listDirectory)
import ErrorMessages
import Utils

-- alias para tipos usados
type ParsecType = ParsecT [Token] CCureState IO(Type, [Token])
type ParsecTokenType = ParsecT [Token] CCureState IO (Token)

-- parsers para os não-terminais
program :: ParsecT [Token] CCureState IO ([Token])
program = do
            a <- declarations

            updateState (turnExecOff)
            e <- subprogramsDeclarations
            -- Desligar exec para chamar funcao
            updateState (addToScopeStack "program")
            updateState (turnExecOn)
            b <- programToken
            c <- stmts
            d <- endToken
            s <- getState
            updateState (symtable_remove_scope (getCurrentScope s) (getCurrentDepth s))
            updateState (removeFromScopeStack)
            eof
            -- s <- getState
            return (a ++ [b] ++ c ++ [d])

subprogramsDeclarations :: ParsecT [Token] CCureState IO ([Token])
subprogramsDeclarations = try (do
                a <- subprogramsToken
                b <- subprograms
                c <- endSubprogramsToken
                return ([a] ++ b ++ [c]))
                <|> return []

subprograms :: ParsecT [Token] CCureState IO ([Token])
subprograms = do
                first <- subprog
                next <- remainingSubprograms
                return (first ++ next)

remainingSubprograms :: ParsecT [Token] CCureState IO ([Token])
remainingSubprograms = (do
                        a <- subprog
                        b <- remainingSubprograms
                        return (a ++ b)) <|> return ([])

subprog :: ParsecT [Token] CCureState IO ([Token])
subprog = do
            a <- functionDecl <|> procDecl
            return a

-- Vou tentar uma coisa
procDecl :: ParsecT [Token] CCureState IO ([Token])
procDecl = do
                a <- procToken
                b <- idToken
                c <- openParentToken
                d <- parDecs --parDecs -- fazer
                e <- closeParentToken
                -- f <- arrowToken
                -- g <- typeToken <|> typeIdToken
                h <- stmts
                i <- endProcToken

                s <- getState
                
                -- verify if has duplicate params 
                let paramsNames = [x | (x, _, _) <- fst d]
                if (hasDuplicateParams paramsNames) then fail duplicateParamsErrorProc
                else do
                  updateState (insertUserProc (b, h ++ [i], fst d))
                  return (a:b:[c] ++ (snd d) ++ [e] ++ h ++ [i])

procedureCall :: Token -> ParsecT [Token] CCureState IO([Token])
procedureCall id = do
                  a <- openParentToken
                  b <- args
                  c <- closeParentToken
                  d <- semiColonToken
                  s <- getState


                  if(execOn s) then do
                    if(not $ isInUserProcs id s) then fail $ invalidProcedureCall id
                    else do

                      let proce = getUserProc id s
                      
                      
                      let tchan = (compatibleArgsP 1 (fst b) proce)
                      let formalArgs = extractThird3 proce
                      if((extractFirst4 tchan) /= 0) then fail (invalidArgsProcedure tchan (length (fst b)) (length (formalArgs)))
                      else do
                        nextStmts <- getInput
                        setInput (getBodyFromProc proce ++ nextStmts)
                        execProc proce (fst b)

                        return (id:a:(snd b) ++ [c] ++[d])


                      --return (getReturnType func, id:a ++ (snd b) ++ c)
                  else
                    return (id:a:(snd b) ++ [c] ++ [d])

------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------

makeArgsToInsertProc :: [(Token, Type, Bool)] -> [(Token, String, Int, Type)] -> String -> Int -> [(Token, String, [(Int, Type)])]
makeArgsToInsertProc [] [] _ _ = []
makeArgsToInsertProc ((id, t, isRef):xs) ((_, _, _, y):ys) name depth =
  if(not isRef) then (id, name, [(depth, y)]) : makeArgsToInsertProc xs ys name depth
  else makeArgsToInsertProc xs ys name depth

-- recebe: argumentos formais, argumentos reais buffados e retorna lista de tuplas em que tupla1 = formal, tupla2 = real
makeArgsToFake :: [(Token, Type, Bool)] -> [(Token, String, Int, Type)] -> String -> Int -> [((Token, String, Int), (Token, String, Int))]
makeArgsToFake [] [] _ _ = []
makeArgsToFake ((id, _, isRef):xs) ((ynome, yscop, ydep, _):ys) name depth =
  if(not isRef) then makeArgsToFake xs ys name depth
  else ((id, name, depth), (ynome, yscop, ydep)) : makeArgsToFake xs ys name depth

------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------

execProc :: UserProc -> [(Token, String, Int, Type)] -> ParsecT [Token] CCureState IO()
execProc (name, pc, formalArgs) realArgs =
    do
      s <- getState

      updateState (addDepth)
      updateState (pushNewScopeStack $ getStringFromIdToken name)
      s <- getState

      let argsToInsert = makeArgsToInsertProc formalArgs realArgs (getStringFromIdToken name) (getCurrentDepth s)
      updateState (insertArgs argsToInsert)
      let argsToFake = makeArgsToFake formalArgs realArgs (getStringFromIdToken name) (getCurrentDepth s)
      let argsFake = [(x) | (x, _) <- argsToFake]
      let argsReal = [(y) | (_, y) <- argsToFake]
      -- Fakeia os argumentos reais
      updateState ( symtable_update_fake_many argsReal argsFake)

      -- updateState(symtable_insert (Id "$ret" (0, 0), getStringFromIdToken name, [(getCurrentDepth s, ret)]))

      a <- stmts
      b <- endProcToken

      s <- getState
      -- if(execOff s) then fail "function must return a value"
      -- else do
      -- let retornou = getMayb $ symtable_get (Id "$ret" (0, 0), getCurrentDepth s) s
      -- if(not $ compatible (ret, []) (retornou, [])) then fail "type error on function return"
      -- else do

      -- DesFakeia os argumentos reais
      updateState ( symtable_update_fake_many argsFake argsReal)

      updateState (symtable_remove_scope (getCurrentScope s) (getCurrentDepth s))
      updateState (removeDepth)
      updateState (removeFromScopeStack)
      updateState (turnExecOn)
      -- return (retornou)

-- Estou tentando uma coisa

functionDecl :: ParsecT [Token] CCureState IO ([Token])
functionDecl = do
                a <- funToken
                b <- idToken
                c <- openParentToken
                d <- parDecs --parDecs -- fazer
                e <- closeParentToken
                f <- arrowToken
                g <- typeToken <|> typeIdToken
                h <- stmts
                i <- endFunToken

                s <- getState

                let paramsToParse = [(x, y) | (x, y, _) <- fst d]
                let paramsNames = [x | (x, _, _) <- fst d]

                if (hasDuplicateParams paramsNames) then fail duplicateParamsError
                else do
                  if(isRegister g) then do
                    if(not $ isInUserTypes g s) then fail (invalidUserTypeReturn g)
                    else do
                      updateState (insertUserFunction (b, h ++ [i], getUserType g s, paramsToParse))
                      return (a:b:[c] ++ (snd d) ++ [e] ++ [f] ++ [g] ++ h ++ [i])
                  else do
                    updateState (insertUserFunction (b, h ++ [i], tokenToType2 g, paramsToParse))
                    return (a:b:[c] ++ (snd d) ++ [e] ++ [f] ++ [g] ++ h ++ [i])

isRegister :: Token -> Bool
isRegister (TypeId id p) = True
isRegister _ = False

parDecs :: ParsecT [Token] CCureState IO ([(Token, Type, Bool)], [Token])
parDecs = try (do
              a <- parDec
              b <- remainingParDecs
              return ((fst a) ++ (fst b), (snd a) ++ (snd b)))
              <|> return ([], [])

remainingParDecs :: ParsecT [Token] CCureState IO ([(Token, Type, Bool)],[Token])
remainingParDecs = (do
                    comma <- commaToken
                    a <- parDec
                    b <- remainingParDecs
                    return ((fst a) ++ (fst b), [comma] ++ (snd a) ++ (snd b)))
                    <|> return ([], [])

parDec :: ParsecT [Token] CCureState IO ([(Token, Type, Bool)],[Token])
parDec = do
            try (do
                ref <- refToken
                a <- typeToken <|> typeIdToken
                b <- idToken

                s <- getState
                if(isRegister a) then do
                  if(not $ isInUserTypes a s) then fail (invalidUserTypeParameter a)
                  else
                    return ([(b, getUserType a s, True)], [ref, a, b])
                else
                  return ([(b, tokenToType2 a, True)], [ref, a, b])
              ) <|>
              (do
                  a <- typeToken <|> typeIdToken
                  b <- idToken

                  s <- getState
                  if(isRegister a) then do
                    if(not $ isInUserTypes a s) then fail (invalidUserTypeParameter a)
                    else
                      return ([(b, getUserType a s, False)], [a, b])
                  else
                    return ([(b, tokenToType2 a, False)], [a, b]))

declarations :: ParsecT [Token] CCureState IO ([Token])
declarations = do
                  a <- typeDeclarations
                  -- b <- globalVariables
                  return a

typeDeclarations :: ParsecT [Token] CCureState IO ([Token])
typeDeclarations = try (do
                      a <- typeDeclarationsToken
                      b <- typeDecs
                      c <- endTypeDeclarationsToken
                      return ([a] ++ b ++ [c]))
                    <|> return []

typeDecs :: ParsecT [Token] CCureState IO ([Token])
typeDecs = do
            first <- typeDec
            next <- remainingTypeDecs
            return (first ++ next)

remainingTypeDecs :: ParsecT [Token] CCureState IO([Token])
remainingTypeDecs = (do
                  a <- typeDec
                  b <- remainingTypeDecs
                  return (a ++ b)) <|> return ([])

typeDec :: ParsecT [Token] CCureState IO ([Token])
typeDec = do
            a <- registerToken
            b <- typeIdToken
            s <- getState
            if (isInUserTypes b s) then fail (alreadyDeclaredRegisterError (getStringFromIdToken b))
            else do
              updateState (insertUserType b)
              c <- varDeclsRegister b
              d <- endRegisterToken
              return (a:[b] ++ c ++ [d])

varDeclsRegister :: Token -> ParsecT [Token] CCureState IO ([Token])
varDeclsRegister id = do
                        first <- attributeRegisterParser id
                        next <- remainingvarDeclsRegister id
                        return (first ++ next)

remainingvarDeclsRegister :: Token -> ParsecT [Token] CCureState IO([Token])
remainingvarDeclsRegister id = (do
                                  a <- attributeRegisterParser id
                                  b <- remainingvarDeclsRegister id
                                  return (a ++ b)) <|> return ([])

attributeRegisterParser :: Token -> ParsecT [Token] CCureState IO ([Token])
attributeRegisterParser id = do
                        a <- typeToken
                        b <- idToken
                        c <- assignToken
                        d <- expression
                        e <- semiColonToken
                        s <- getState


                        -- let j = removeQuotes d -- Usado para tirar "" de string
                        if (not (compatible_varDecl a d)) then fail (typeErrorMessage a (fst d))
                        else
                          do
                            s <- getState
                            -- if the attribute is already in register, then fail
                            if(isRegAttr b (getUserType id s)) then fail (alreadyDeclaredAttrError (getStringFromIdToken b))
                            else do
                              updateState ( addAttrToUserTypes id (b, fst d) )
                              s <- getState
                              return (a:b:[c] ++ (snd d) ++ [e])


registerAccess :: Token -> ParsecT [Token] CCureState IO(Type, [Token])
registerAccess id = do
                      a <- arrowToken
                      b <- idToken
                      s <- getState
                      s <- getState

                      if(execOn s) then do
                        s <- getState
                        let currDepth = getCurrentDepth s
                        let reg = getMayb (symtable_get (id, currDepth) s)
                        -- Verificar se reg é um RegisterType
                        if(not $ isRegisterType reg) then do error (isNotRegisterType id)
                        else do
                          -- Verificar se b é um atributo de reg
                          if(not $ isRegAttr b reg ) then fail (invalidRegisterAccess (typeToToken reg) b)
                          else do
                              return (getRegAttr b reg, id:a:[b])
                      else do
                        return (NULL, id:a:[b])

registerAssign :: Token -> ParsecT [Token] CCureState IO([Token])
registerAssign id = do
                      a <- arrowToken
                      b <- idToken
                      c <- assignToken
                      d <- expression
                      e <- semiColonToken
                      s <- getState

                      if(execOn s) then do
                        let currDepth = getCurrentDepth s
                        let reg = getMayb (symtable_get (id, currDepth) s)
                        -- Verificar se reg é um RegisterType
                        if(not $ isRegisterType reg) then fail (isNotRegisterType id)
                        else do
                          -- Verificar se b é um atributo de reg
                          if(not $ isRegAttr b reg ) then fail (invalidRegisterAccess (typeToToken reg) b)
                          else
                            if (not (compatible (getRegAttr b reg, []) d)) then fail (typeErrorMessage (typeToToken(getRegAttr b reg)) (fst d))
                            else
                              do
                                updateState (symtable_update_reg_attr (id, currDepth, b, fst d))
                                return (id:a:b:[c] ++ (snd d) ++ [e])
                      else
                        return (id:a:b:[c] ++ (snd d) ++ [e])


typeToken :: ParsecT [Token] CCureState IO(Token)
typeToken = try intToken <|> doubleToken <|> boolToken <|> stringToken


-- matrixSizeParser :: ParsecT [Token] CCureState IO(Type, [Token])
-- matrixSizeParser = try (do
--                           val <- intLitToken
--                           return (tokenToType val, [val]))
--                         <|> variableParser

matrixExpr :: ParsecT [Token] CCureState IO(Type, [Token])
matrixExpr = (do
              a <- expiMatrix
              result <- eval_remaining_matrix a matrixOpToken expiMatrix
              return result
            )

enclosedMatrixExp :: ParsecT [Token] CCureState IO(Type, [Token])
enclosedMatrixExp = do
                a <- openParentToken
                b <- matrixExpr
                c <- closeParentToken
                return b

expiMatrix :: ParsecT [Token] CCureState IO(Type, [Token])
expiMatrix = try (do
                    x <- intLitToken <|> doubleLitToken
                    return (tokenToType x, [x])
                  ) <|> variableParser <|> enclosedMatrixExp

matrixOpToken :: ParsecT [Token] CCureState IO(Token)
matrixOpToken = try multMatrixToken <|> plusMatrixToken <|> plusToken <|> multToken

matrixAcces :: Token -> ParsecT [Token] CCureState IO(Type, [Token])
matrixAcces id = do
                ob1 <- openBrackToken
                x   <- expression
                cb1 <- closeBrackToken
                ob2 <- openBrackToken
                y   <- expression
                cb2 <- closeBrackToken

                s <- getState
                if(execOn s) then do
                  let currDepth = getCurrentDepth s
                  let matrix = getMayb (symtable_get (id, currDepth) s)
                  if not $ validAccess (fst x) (fst y) then fail matrixPositiveIntegersAccess
                  else
                    if(not $ canAccesMatrix (fst x) (fst y) matrix) then fail matrixAccessOutOfBounds
                    else
                      return (getValFromMatrix (fst x) (fst y) matrix, id:[ob1] ++ (snd x) ++ cb1:[ob2] ++ (snd y) ++ [cb2])
                else
                  return (NULL, id:[ob1] ++ (snd x) ++ cb1:[ob2] ++ (snd y) ++ [cb2])

eval_remaining_matrix :: (Type, [Token]) -> ParsecTokenType -> ParsecType -> ParsecT [Token] CCureState IO(Type, [Token])
eval_remaining_matrix m1 operator remain = (do
                                op <- operator
                                m2 <- remain
                                s <- getState
                                if (execOn s) then do
                                  if (not (compatible_op m1 op m2)) then fail (invalidOperation (typeToToken(fst m1)) op (typeToToken(fst m2)))
                                  else
                                    do
                                      result <- eval_remaining_matrix (eval m1 op m2 (execOn s)) operator remain
                                      return (result)
                                else do
                                  result <- eval_remaining_matrix (eval m1 op m2 (execOn s)) operator remain
                                  return (result))
                              <|> return m1

matrixDecl :: ParsecT [Token] CCureState IO([Token])
matrixDecl = do
              -- matrix<linha, coluna, tipo(int ou double)> m = 0;
              mat <- matrixToken

              l <- lessToken
              lin <- expression
              a <- commaToken

              col <- expression
              b <- commaToken

              typ <- intToken <|> doubleToken
              r <- greatToken

              id <- idToken
              assig <- assignToken

              initVal <- matrixExpr <|> (do
                                          x <- intLitToken <|> doubleLitToken
                                          return (tokenToType x, [x]))
              sc <- semiColonToken

              s <- getState

              if(execOn s) then do
                -- aq tem q checar se o tipo é valido, ou seja:
                -- se lin e col sao inteiros > 0
                if(not $ validDimensions (fst lin) (fst col)) then fail matrixPositiveIntegersDimensions
                else
                  -- se initVal é compatível com a matriz
                  if(not $ compatible_matrix_assign typ initVal (fst lin) (fst col)) then fail matrixIncompatibleValueAssigned
                  else
                  -- se tiver tudo ok, construir a matriz e colocar na tabela de simbolos
                    do
                      s <- getState
                      let currDepth = getCurrentDepth s
                      let matrixToSave = makeMatrixType (fst lin) (fst col) (fst initVal)
                      updateState (symtable_insert (id, getCurrentScope s, [(currDepth, matrixToSave)]))
                      return  (mat:[l] ++ (snd lin) ++ [a] ++ (snd col) ++ b:typ:r:id:[assig] ++ (snd initVal) ++[sc])
              else
                return (mat:[l] ++ (snd lin) ++ [a] ++ (snd col) ++ b:typ:r:id:[assig] ++ (snd initVal) ++[sc])

registerDecl :: ParsecT [Token] CCureState IO([Token])
registerDecl = do
              a <- typeIdToken
              b <- idToken
              c <- assignToken
              try (do
                  d <- defaultParser a b
                  e <- semiColonToken
                  return (a:b:[c] ++ d ++ [e]))
                  <|> do
                      d <- expression
                      e <- semiColonToken
                      s <- getState
                      if(execOn s) then do
                        if(not $ isInUserTypes a s) then fail (invalidUserType a)
                        else do
                          
                          -- Check if b was already declared
                          if(isInSymTable (b, (getCurrentDepth s), getCurrentScope s) s) then fail (alreadyDeclaredError (getStringFromIdToken b))
                          else do

                            -- Check if d is compatible with a
                            if(not $ compatible (getUserType a s, []) d) then fail (typeErrorMessage a (fst d))
                            else do

                              let valToInsert = (fst d)
                              let currentDepth =  getCurrentDepth s
                              updateState (symtable_insert (b, getCurrentScope s, [(currentDepth, valToInsert)]))
                              return (a:b:[c] ++ (snd d) ++ [e])
                      else
                        return (a:b:[c] ++ (snd d) ++ [e])

defaultParser :: Token -> Token -> ParsecT [Token] CCureState IO([Token])
defaultParser typeid id = do
                          a <- defaultToken
                          b <- openParentToken
                          c <- typeIdToken
                          d <- closeParentToken
                          s <- getState
                          if (execOn s) then
                            if (not (compareTypeIdTokens typeid c)) then fail (typeErrorDefaultRegister typeid c)
                            else
                              -- Check if typeid is declared on UserTypes
                              if(not (isInUserTypes typeid s)) then fail (invalidUserType typeid)
                              else
                                -- Check if id is already declared in symtable
                                if(isInSymTable (id, (getCurrentDepth s), getCurrentScope s) s) then fail (alreadyDeclaredError (getStringFromIdToken id))
                                else do
                                  let user = getUserType typeid s
                                  let currentDepth =  getCurrentDepth s
                                  updateState (symtable_insert (id, getCurrentScope s, [(getCurrentDepth s, user)]))
                                  return (a:b:c:[d])
                          else
                            return (a:b:c:[d])


varDecl :: ParsecT [Token] CCureState IO([Token])
varDecl = do
            a <- typeToken
            b <- idToken
            c <- assignToken
            d <- expression
            s <- getState

            if(execOn s) then do
              -- let j = removeQuotes d -- Usado para tirar "" de string
              if (not (compatible_varDecl a d)) then fail $ typeErrorMessage a (fst d) 
              else do
                if(isInSymTable (b, (getCurrentDepth s), getCurrentScope s) s) then fail (alreadyDeclaredError (getStringFromIdToken b))
                else do
                  s <- getState
                  let currentDepth =  getCurrentDepth s
                  updateState (symtable_insert (b, getCurrentScope s, [(currentDepth, fst d)]))
                  s <- getState
                  e <- semiColonToken
                  return (a:b:[c] ++ (snd d) ++ [e])
            else do
              e <- semiColonToken
              return (a:b:[c] ++ (snd d) ++ [e])

-- removeQuotes :: (Type, [Token]) -> (Type, [Token])
-- removeQuotes ( StringType x , b) = ( StringType  x , b)
-- removeQuotes (a, b) = (a, b)

stmts :: ParsecT [Token] CCureState IO([Token])
stmts = do
          first <- stmt
          next <- remainingStmts
          return (first ++ next)

stmt :: ParsecT [Token] CCureState IO([Token])
stmt = varDecl <|> matrixDecl <|> registerDecl <|> assign <|> printPuts <|> readStup <|> whileStmt <|> ifStmt <|> breakStmt <|> returnStmt

remainingStmts :: ParsecT [Token] CCureState IO([Token])
remainingStmts = (do
                  a <- stmt
                  b <- remainingStmts
                  return (a ++ b)) <|> return ([])

printPuts :: ParsecT [Token] CCureState IO([Token])
printPuts = do
              a <- putsToken
              b <- openParentToken
              c <- expression
              d <- closeParentToken
              e <- semiColonToken
              s <- getState
              if(execOn s) then do
                liftIO (putStr $ show (fst c))
                liftIO (hFlush stdout)
              else pure ()
              return (a:[b] ++ (snd c) ++ d:[e])

matrixStup :: Token -> ParsecT [Token] CCureState IO([Token])
matrixStup id = do
                  ob1 <- openBrackToken
                  (xt, xx)   <- expression
                  cb1 <- closeBrackToken
                  ob2 <- openBrackToken
                  (yt, yy)   <- expression
                  cb2 <- closeBrackToken

                  d <- commaToken
                  e <- typeToken
                  f <- closeParentToken
                  g <- semiColonToken
                  s <- getState

                  if(execOn s) then do
                    input <- liftIO (getLine)

                    -- Check if id is on symtable
                    if(not $ isInSymTable (id, (getCurrentDepth s), getCurrentScope s) s) then fail (notDeclaredError (getStringFromIdToken id))
                    else do
                      -- Check if string from input can be converted to e
                      if(not $ canConvertStringToType input e) then fail (typeErrorInput e input)
                      else do
                        let strConverted = convertStringToType input e
                        let currentDepth = getCurrentDepth s
                        let matrix = getMayb (symtable_get (id, currentDepth) s)
                        if not $ validAccess xt yt then fail matrixPositiveIntegersAccess
                        else
                          if(not $ canAccesMatrix xt yt matrix) then fail matrixAccessOutOfBounds
                          else
                            if(not (compatible_matrix (get_type id s, []) (strConverted, []) )) then fail (typeErrorMessage (typeToToken(get_type id s)) strConverted)
                            else do
                              updateState (symtable_update_matrix (id, currentDepth, getValFromType xt, getValFromType yt, strConverted))
                              return (id:[ob1] ++ xx ++ cb1:[ob2] ++ yy ++ [cb2])
                  else
                    return (id:[ob1] ++ xx ++ cb1:[ob2] ++ yy ++ [cb2])

getValFromType :: Type -> Int
getValFromType (IntType v) = v
getValFromType _           = 0

getStringFromIdToken :: Token -> String
getStringFromIdToken (Id x _) = x
getStringFromIdToken (TypeId x _) = x
getStringFromIdToken _        = ""

readStup :: ParsecT [Token] CCureState IO([Token])
readStup = do
              a <- stupToken
              b <- openParentToken
              c <- idToken
              try (do
                    matrixStup c
                ) <|> do
                        d <- commaToken
                        e <- typeToken
                        f <- closeParentToken
                        g <- semiColonToken
                        s <- getState
                        if(execOn s) then do
                          input <- liftIO (getLine)

                          -- Check if id is already declared
                          if(not $ isInSymTable (c, (getCurrentDepth s), getCurrentScope s) s) then fail (notDeclaredError (getStringFromIdToken c))
                          else do
                            -- Check if string from input can be converted to e
                            if(not $ canConvertStringToType input e) then fail (typeErrorInput e input)
                            else do
                              let strConverted = convertStringToType input e
                              if(not (compatible (get_type c s, []) (strConverted, []) )) then fail (typeErrorMessage (typeToToken(get_type c s)) strConverted)
                              else do
                                updateState (symtable_update (c, getCurrentDepth s ,strConverted))
                                return (a:b:c:d:e:f:[g])

                        else return (a:b:c:d:e:f:[g])

canConvertStringToType :: String -> Token -> Bool
canConvertStringToType x (Double _) = isRight (readDouble x)
canConvertStringToType x (Int _) = isRight (readInt x)
canConvertStringToType x (Bool _) = (x == "True") || (x == "False")
canConvertStringToType x (Str _) = True
canConvertStringToType _ _ = False

convertStringToType :: String -> Token -> Type
convertStringToType x (Double _) =
      case (readDouble x) of
            Right num -> (DoubleType num)
            Left err  -> error "could not convert input to double value"
convertStringToType x (Int _) =
      case (readInt x) of
            Right num -> (IntType num)
            Left err  -> error "could not convert input to int value"
convertStringToType x (Bool _) =
      if (x == "True") then (BoolType True)
      else do
        if (x == "False") then (BoolType False)
        else error "could not convert input to boolean value"
convertStringToType x (Str _) = (StringType x)

-- Função para ler e converter uma String em Int
readInt :: String -> Either String Int
readInt str = readEither str

-- Função para ler e converter uma String em Double
readDouble :: String -> Either String Double
readDouble str = readEither str

ifStmt :: ParsecT [Token] CCureState IO([Token])
ifStmt = do
            a <- ifToken
            b <- enclosed_exp
            s <- getState

            -- if (exp)

            -- else <|> []

            -- endIf

            if(execOn s) then
              if( not (compatible b (BoolType True, []) ) ) then fail (controlNotBoolError (typeToToken(fst b)))
              else do
                if(get_bool_value (fst b)) then do -- se a condicao do if eh true
                  updateState (addToScopeStack "if") -- adiciona o escopo do if
                 
                  c <- stmts -- executa os stmts
                  
                  s <- getState
                  if(execOn s) then do 
                    updateState(turnExecOff) -- desativa a execução pra passar pelo else

                    d <- elseOrNothingStmt -- passa pelo else com exec off
                    e <- endIfToken -- fim do if
                      
                    updateState(turnExecOn) -- liga a exec dnv
                    
                    s <- getState
                    updateState (symtable_remove_scope (getCurrentScope s) (getCurrentDepth s))
                    updateState(removeFromScopeStack)
                    return (a:(snd b) ++ c ++ d ++ [e])
                  else do
                    d <- elseOrNothingStmt -- passa pelo else com exec off
                    e <- endIfToken -- fim do if
                    updateState (symtable_remove_scope (getCurrentScope s) (getCurrentDepth s))
                    updateState(removeFromScopeStack)
                    return (a:(snd b) ++ c ++ d ++ [e])


                else do -- se a condicao do if eh false
                  updateState (turnExecOff) -- desativa a exec pra passar pelo corpo do if

                  c <- stmts -- passa pelo corpo do if

                  updateState(turnExecOn) -- liga a exec pra executar o else ou ir simbora
                  updateState (addToScopeStack "else") -- adiciona o else no escopo
                  d <- elseOrNothingStmt
                  e <- endIfToken
                  
                  s <- getState
                  updateState (symtable_remove_scope (getCurrentScope s) (getCurrentDepth s))
                  updateState (removeFromScopeStack) -- remove o else do escopo
                  
                  return (a:(snd b) ++ c ++ d ++ [e]) 
            else do
              c <- stmts
              d <- elseOrNothingStmt
              e <- endIfToken
              return (a:(snd b) ++ c ++ d ++ [e])

elseOrNothingStmt :: ParsecT [Token] CCureState IO([Token])
elseOrNothingStmt = (do
                      a <- elseToken
                      s <- getState
                      if(execOn s) then do -- se a exec tiver on  
                        b <- stmts -- executa o corpo do else
                        return (a:b)
                      else do
                        b <- stmts
                        return (a:b))
                      <|> return []
                      
whileStmt :: ParsecT [Token] CCureState IO([Token])
whileStmt = do
              -- s <- getState
              pc <- getInput
              a <- whileToken
              b <- enclosed_exp

              s <- getState

              if(execOn s) then do
                updateState (addToScopeStack "while")
                updateState (addToLoopStack OK)
                if( not (compatible b (BoolType True, []) ) ) then fail (controlNotBoolError (typeToToken(fst b)))
                else
                  if(get_bool_value (fst b)) then do
                    c <- stmts
                    d <- endWhileToken


                    s <- getState
                    -- Se apos os stmts o exec tiver off, leu um break ou continue ou return
                    if(not (execOn s)) then do
                      -- Se foi um break, acaba o loop
                      if((getCurrentLoopStatus s) == BREAK) then do
                        updateState (turnExecOn)
                        s <- getState
                        updateState (symtable_remove_scope (getCurrentScope s) (getCurrentDepth s))
                        updateState (removeFromScopeStack)
                        updateState (removeFromLoopStack)
                        return ([a] ++ (snd b) ++ c ++ [d])
                      else do
                        -- Se foi um return, acaba o loop
                        s <- getState
                        updateState (symtable_remove_scope (getCurrentScope s) (getCurrentDepth s))
                        updateState (removeFromScopeStack)
                        updateState (removeFromLoopStack)
                        return ([a] ++ (snd b) ++ c ++ [d])
                      -- Se foi um continue, continua o loop
                    else do
                      setInput (pc)
                      s <- getState
                      updateState (symtable_remove_scope (getCurrentScope s) (getCurrentDepth s))
                      updateState (removeFromScopeStack)
                      updateState (removeFromLoopStack)
                      return ([a] ++ (snd b) ++ c ++ [d])
                  else do
                    -- Desativo a execução aqui
                    updateState (turnExecOff)
                    c <- stmts
                    d <- endWhileToken
                    -- Ativo a execução aqui
                    updateState (turnExecOn)
                    s <- getState
                    updateState (removeFromScopeStack)
                    updateState (removeFromLoopStack)
                    return ([a] ++ (snd b) ++ c ++ [d])
              else do
                c <- stmts
                d <- endWhileToken
                return ([a] ++ (snd b) ++ c ++ [d])

breakStmt :: ParsecT [Token] CCureState IO([Token])
breakStmt = do
              a <- breakToken
              b <- semiColonToken

              s <- getState
              if(execOn s) then do


                -- Checar se estou dentro de um loop
                s <- getState


                if(isInCurrentScope "while" s) then do
                  updateState (turnExecOff)
                  -- updateState((addToLoopStack.removeFromLoopStack) BREAK)
                  updateState (removeFromLoopStack)
                  updateState (addToLoopStack BREAK)
                  -- if((getCurrentLoopStatus s) == OK) then
                  --   updateState(addToLoopStack BREAK)
                  -- else pure()
                else
                  fail breakOutOfLoopError
              else pure ()

              return (a:[b])

returnStmt :: ParsecT [Token] CCureState IO([Token])
returnStmt = do
              a <- returnToken
              b <- expression
              c <- semiColonToken
              s <- getState
              if(execOn s) then do
                let lastScop = getLastScop (getCurrentScope s)
                if(lastScop == "program" || isInUserProcs (Id lastScop (0,0)) s) then fail returnOutOfFunctionError
                else do
                  let ret = getReturnType (getCurrentScope s) s
                  if(not (compatible b (ret, []))) then fail (typeErrorReturn (typeToToken ret) (fst b))
                  else do
                    updateState (symtable_update (Id "$ret" (0, 0), getCurrentDepth s, fst b))
                    updateState (turnExecOff)
                    return (a:(snd b) ++ [c])
              else
                return (a:(snd b) ++ [c])

getReturnType :: String -> CCureState -> Type
getReturnType scope s = getMayb (symtable_get (Id "$ret" (0, 0), getCurrentDepth s) s)

assign :: ParsecT [Token] CCureState IO([Token])
assign = do
          a <- idToken
          try (registerAssign a)
            <|> (do
              c <- procedureCall a
              return c)
            <|> (do
                  b <- assignToken
                  c <- expression
                  d <- semiColonToken
                  s <- getState
                  if(execOn s) then do
                    -- Check if a was already declared
                    if(not $ isInSymTable (a, (getCurrentDepth s), getCurrentScope s) s) then fail (notDeclaredError (getStringFromIdToken a))
                    else do
                    -- let j = removeQuotes c -- Usado para tirar "" de string
                      if (not (compatible (get_type a s, []) c)) then fail (typeErrorMessage (typeToToken(get_type a s)) (fst c))
                      else
                        do
                          updateState (symtable_update (a, getCurrentDepth s, fst c))
                          -- s <- getState
                          return (a:[b] ++ (snd c) ++ [d])
                  else
                    return (a:[b] ++ (snd c) ++ [d]))

-- expression :: ParsecT [Token] CCureState IO(Token)
-- expression = try bin_expression <|> una_expression

expression :: ParsecT [Token] CCureState IO(Type, [Token])
expression = bool_expression

bool_expression :: ParsecT [Token] CCureState IO(Type, [Token])
bool_expression = and_expression

and_expression :: ParsecT [Token] CCureState IO(Type, [Token])
and_expression = (do
                    a <- or_expression
                    result <- eval_remaining a andToken or_expression
                    return result)

or_expression :: ParsecT [Token] CCureState IO(Type, [Token])
or_expression = (do
                    a <- neg_expression
                    result <- eval_remaining a orToken neg_expression
                    return result)

neg_expression :: ParsecT [Token] CCureState IO(Type, [Token])
neg_expression = (do
                    op <- negToken
                    rel <- rel_expression
                    s <- getState
                    if(execOn s) then do
                      if(not $ compatible_op_unary op rel) then fail (invalidUnaryOperation op (typeToToken (fst rel)))
                      else
                        return (eval_unary op rel (execOn s))
                    else
                      return (eval_unary op rel (execOn s))) <|>
                  (do
                    a <- rel_expression
                    return a)

rel_operator :: ParsecT [Token] CCureState IO(Token)
rel_operator = try eqToken <|> diffToken <|> leqToken <|> geqToken <|> lessToken <|> greatToken

rel_expression :: ParsecT [Token] CCureState IO(Type, [Token])
rel_expression = (do
                    a <- sum_expression
                    result <- eval_remaining a rel_operator sum_expression
                    return result)

sum_minus :: ParsecT [Token] CCureState IO(Token)
sum_minus = try plusToken <|> minusToken

sum_expression :: ParsecT [Token] CCureState IO(Type, [Token])
sum_expression = (do
                    a <- unary_minus_expression
                    result <- eval_remaining a sum_minus unary_minus_expression
                    return result)

unary_minus_expression :: ParsecT [Token] CCureState IO(Type, [Token])
unary_minus_expression = (do
                            op <- minusToken
                            t <- term
                            s <- getState
                            return (eval_unary op t (execOn s))) <|>
                         (do
                            a <- term
                            return a)

mult_div_mod :: ParsecT [Token] CCureState IO(Token)
mult_div_mod = try multToken <|> diviToken <|> modToken

term :: ParsecT [Token] CCureState IO(Type, [Token])
term = (do
          a <- factor
          result <- eval_remaining a mult_div_mod factor
          return result)

factor :: ParsecT [Token] CCureState IO(Type, [Token])
factor = (do
            a <- expi
            result <- eval_remaining_right a expoToken expi
            return result
          )

enclosed_exp :: ParsecT [Token] CCureState IO(Type, [Token])
enclosed_exp = do
                a <- openParentToken
                (bType, bTokens) <- expression
                c <- closeParentToken
                return (bType, a:bTokens ++ [c])

numTypeToken :: ParsecT [Token] CCureState IO(Token)
numTypeToken = try intToken <|> doubleToken

castParser :: ParsecT [Token] CCureState IO(Type, [Token])
castParser = do
              ct <- castToken
              openP <- openParentToken
              (valExp, exp) <- expression
              c <- commaToken
              t <- numTypeToken
              closeP <- closeParentToken
              s <- getState
              if(execOn s) then do
                if (not (canCast valExp t)) then fail (castError (typeToToken valExp) t)
                else
                  do
                    return (cast valExp t, ct:[openP] ++ exp ++ c:t:[closeP])
              else
                return (NULL, ct:[openP] ++ exp ++ c:t:[closeP])

canCast :: Type -> Token -> Bool
canCast (IntType _) (Int _)       = True
canCast (IntType _) (Double _)    = True
canCast (DoubleType _) (Double _) = True
canCast _ _                        = False

cast :: Type -> Token -> Type
cast (IntType v) (Int _)       = IntType v
cast (IntType v) (Double _)    = DoubleType (fromIntegral v)
cast (DoubleType v) (Double _) = DoubleType v

expi :: ParsecT [Token] CCureState IO(Type, [Token])
expi = try (do
        x <- intLitToken <|> doubleLitToken <|> boolLitToken <|> stringLitToken
        return (tokenToType x, [x])) <|> castParser <|> enclosed_exp <|> variableParser

tokenToType :: Token -> Type
tokenToType (IntLit v _)    = IntType v
tokenToType (DoubleLit v _) = DoubleType v
tokenToType (BoolLit v _)   = BoolType v
tokenToType (StringLit v _) = StringType v
tokenToType _               = undefined

tokenToType2 :: Token -> Type
tokenToType2 (Int _)    = IntType 0
tokenToType2 (Double _) = DoubleType 0.0
tokenToType2 (Bool _)   = BoolType True
tokenToType2 (Str _)    = StringType " "
tokenToType2 _          = undefined

compareTypeIdTokens :: Token -> Token -> Bool
compareTypeIdTokens (TypeId x _) (TypeId y _) = x == y
compareTypeIdTokens _ _ = False

functionCall :: Token -> ParsecT [Token] CCureState IO(Type, [Token])
functionCall id = do
                  a <- openParentToken
                  b <- args
                  c <- closeParentToken
                  s <- getState


                  if(execOn s) then do
                    
                    if(not $ isInUserFunctions id s) then fail (invalidFunctionCall id)
                    else do

                      let func = getUserFunc id s
                      let fstb = [(x) | (_, _, _, x) <- fst b]

                      let tchan = (compatibleArgs 1 (fstb) func)
                      let formalArgs = extractFourth4 func
                    
                      -- if((extractFirst4 tchan) /= 0) then fail (invalidArgsProcedure tchan (length (fst b)) (length (formalArgs)))

                      if((extractFirst4 tchan) /= 0) then fail (invalidArgsProcedure tchan (length (fstb)) (length (formalArgs)))
                      else do
                        nextStmts <- getInput
                        setInput (getBodyFromFunc func ++ nextStmts)
                        returnFromFunc <- execFunction func (fstb)


                        return (returnFromFunc, id:a:(snd b) ++ [c])


                      --return (getReturnType func, id:a ++ (snd b) ++ c)
                  else
                    return (NULL, id:a:(snd b) ++ [c])


---------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------

makeArgsToInsert :: [(Token, Type)] -> [Type] -> String -> Int -> [(Token, String, [(Int, Type)])]
makeArgsToInsert [] [] _ _ = []
makeArgsToInsert ((id, t):xs) (y:ys) name depth = (id, name, [(depth, y)]) : makeArgsToInsert xs ys name depth


-- (Token, Int, String)

insertArgs :: [(Token, String, [(Int, Type)])] -> CCureState -> CCureState
insertArgs [] s = s
insertArgs ((id, name, types):xs) s = insertArgs xs (symtable_insert (id, name, types) s)

---------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------

execFunction :: UserFunction -> [Type] -> ParsecT [Token] CCureState IO(Type)
execFunction (name, pc, ret, formalArgs) realArgs =
    do
      updateState (addDepth)
      updateState (pushNewScopeStack $ getStringFromIdToken name)
      s <- getState

      let argsToInsert = makeArgsToInsert formalArgs realArgs (getStringFromIdToken name) (getCurrentDepth s)
      updateState (insertArgs argsToInsert)
      updateState (symtable_insert (Id "$ret" (0, 0), getStringFromIdToken name, [(getCurrentDepth s, ret)]))

      a <- stmts
      b <- endFunToken

      s <- getState
      if(execOn s) then fail "NoValueReturnedError: Got to the end of function execution, but didn't return a value."
      else do
        let retornou = getMayb $ symtable_get (Id "$ret" (0, 0), getCurrentDepth s) s
        if(not $ compatible (ret, []) (retornou, [])) then fail (typeErrorReturn (typeToToken ret) retornou)
        else do
          updateState (symtable_remove_scope (getCurrentScope s) (getCurrentDepth s))
          updateState (removeDepth)
          updateState (removeFromScopeStack)
          updateState (turnExecOn)
          return (retornou)



args :: ParsecT [Token] CCureState IO([(Token, String, Int, Type)],[Token])
args = try (do
            a <- expression
            b <- remainingArgs
            return ((Id "$notref" (0,0), "noscope", -1, fst a):(fst b), (snd a) ++ (snd b))) <|>
            (do
              ref <- refToken
              id <- idToken
              b <- remainingArgs
              -- try
                -- (registerAccess id)
                -- <|>
              s <- getState
              if(execOn s) then
                return ((symtable_get_info id (getCurrentDepth s) s):fst b, [ref, id] ++ snd b)
              else
                return ((Id "$notref" (0,0), "noscope", -1, NULL):fst b, [ref, id] ++ snd b)
            )
            <|> return ([], [])



remainingArgs :: ParsecT [Token] CCureState IO([(Token, String, Int, Type)], [Token])
remainingArgs = try (do
                  comma <- commaToken
                  try (do 
                        a <- expression
                        b <- remainingArgs
                        return ((Id "$notref" (0,0), "noscope", -1, fst a):(fst b), [comma] ++ (snd a) ++ (snd b))) 
                   <|> 
                    (do
                      ref <- refToken
                      id <- idToken
                      b <- remainingArgs
                      -- try
                        -- (registerAccess id)
                        -- <|>
                      s <- getState
                      if(execOn s) then
                        return ((symtable_get_info id (getCurrentDepth s) s): fst b, [comma, ref, id] ++ snd b)
                      else
                        return ((Id "$notref" (0,0), "noscope", -1, NULL): fst b, [comma, ref, id])
                    ))
                  <|> return ([], [])

variableParser :: ParsecT [Token] CCureState IO(Type, [Token])
variableParser = do
                  id <- idToken
                  (matrixAcces id)
                    <|> (registerAccess id)
                    <|> (functionCall id)
                    <|>
                    (do
                      s <- getState
                      if(execOn s) then
                        -- Check if id was not declared
                        if(not $ isInSymTable (id, (getCurrentDepth s), getCurrentScope s) s) then fail (notDeclaredError (getStringFromIdToken id))
                        else do
                          return (getMayb (symtable_get (id, getCurrentDepth s) s), [id])
                      else
                        return (NULL, [id]))


getMayb :: Maybe a -> a
getMayb (Just a) = a
getMayb Nothing = error "GetMayb deu ruim"

--- funções considerando associatividade à esquerda                  
eval_remaining :: (Type, [Token]) -> ParsecTokenType -> ParsecType -> ParsecT [Token] CCureState IO(Type, [Token])
eval_remaining n1 operator remain = (do
                                op <- operator
                                n2 <- remain
                                s <- getState
                                if (execOn s) then do
                                  if (not (compatible_op n1 op n2)) then fail (invalidOperation (typeToToken(fst n1)) op (typeToToken(fst n2)))
                                  else
                                    do
                                      result <- eval_remaining (eval n1 op n2 (execOn s)) operator remain
                                      return (result)
                                else do
                                  result <- eval_remaining (eval n1 op n2 (execOn s)) operator remain
                                  return (result))
                              <|> return n1

eval_remaining_right :: (Type, [Token]) -> ParsecTokenType -> ParsecType -> ParsecT [Token] CCureState IO(Type, [Token])
eval_remaining_right n1 operator remain = (do
                                op <- operator
                                n2 <- remain
                                s <- getState
                                result <- eval_remaining_right n2 operator remain
                                if(execOn s) then do
                                  if (not (compatible_op n1 op n2)) then fail (invalidOperation (typeToToken(fst n1)) op (typeToToken(fst n2)))
                                  else
                                    do
                                      return (eval n1 op result (execOn s))
                                else do
                                  return (eval n1 op result (execOn s)))
                              <|> return n1

eval_unary :: Token -> (Type, [Token]) -> Bool -> (Type, [Token])
eval_unary op a False = (NULL, op:(snd a))                         -- Não importa a saída, pois o exec ta off
eval_unary (Neg p) (BoolType x, a) True = (BoolType (not x), a)
eval_unary (Plus p) (IntType x, a) True = (IntType (x), a)
eval_unary (Plus p) (DoubleType x, a) True = (DoubleType (x), a)
eval_unary (Minus p) (IntType x, a) True = (IntType (-x), a)
eval_unary (Minus p) (DoubleType x, a) True = (DoubleType (-x), a)

eval :: (Type, [Token]) -> Token -> (Type, [Token]) -> Bool -> (Type, [Token])
eval a op b False = (NULL, (snd a) ++ [op] ++ (snd b))                         -- Não importa a saída, pois o exec ta off
eval (IntType x, a) (Plus op) (IntType y, b) True = (IntType (x + y), a ++ [(Plus op)] ++ b)
eval (IntType x, a) (Minus op) (IntType y, b) True = (IntType (x - y), a ++ [(Minus op)] ++ b)
eval (IntType x, a) (Mult op) (IntType y, b) True = (IntType (x * y), a ++ [(Mult op)] ++ b)
eval (IntType x, a) (Divi op) (IntType y, b) True = (IntType (x `div` y), a ++ [(Divi op)] ++ b)
eval (IntType x, a) (Mod op) (IntType y, b) True = (IntType (x `mod` y), a ++ [(Mod op)] ++ b)
eval (IntType x, a) (Expo op) (IntType y, b) True = (IntType (x ^ y), a ++ [(Expo op)] ++ b)
eval (IntType x, a) (Lesser op) (IntType y, b) True = (BoolType (x < y), a ++ [(Lesser op)] ++ b)
eval (IntType x, a) (Greater op) (IntType y, b) True = (BoolType (x > y), a ++ [(Greater op)] ++ b)
eval (IntType x, a) (LessEq op) (IntType y, b) True = (BoolType (x <= y), a ++ [(LessEq op)] ++ b)
eval (IntType x, a) (GreatEq op) (IntType y, b) True = (BoolType (x >= y), a ++ [(GreatEq op)] ++ b)
eval (IntType x, a) (Eq op) (IntType y, b) True = (BoolType (x == y), a ++ [(Eq op)] ++ b)
eval (IntType x, a) (Diff op) (IntType y, b) True = (BoolType (x /= y), a ++ [(Diff op)] ++ b)

eval (DoubleType x, a) (Plus op) (DoubleType y, b) True = (DoubleType (x + y), a ++ [(Plus op)] ++ b)
eval (DoubleType x, a) (Minus op) (DoubleType y, b) True = (DoubleType (x - y), a ++ [(Minus op)] ++ b)
eval (DoubleType x, a) (Mult op) (DoubleType y, b) True = (DoubleType (x * y), a ++ [(Mult op)] ++ b)
eval (DoubleType x, a) (Divi op) (DoubleType y, b) True = (DoubleType (x / y), a ++ [(Divi op)] ++ b)
eval (DoubleType x, a) (Expo op) (DoubleType y, b) True = (DoubleType (x ** y), a ++ [(Expo op)] ++ b)
eval (DoubleType x, a) (Lesser op) (DoubleType y, b) True = (BoolType (x < y), a ++ [(Lesser op)] ++ b)
eval (DoubleType x, a) (Greater op) (DoubleType y, b) True = (BoolType (x > y), a ++ [(Greater op)] ++ b)
eval (DoubleType x, a) (LessEq op) (DoubleType y, b) True = (BoolType (x <= y), a ++ [(LessEq op)] ++ b)
eval (DoubleType x, a) (GreatEq op) (DoubleType y, b) True = (BoolType (x >= y), a ++ [(GreatEq op)] ++ b)
eval (DoubleType x, a) (Eq op) (DoubleType y, b) True = (BoolType (x == y), a ++ [(Eq op)] ++ b)
eval (DoubleType x, a) (Diff op) (DoubleType y, b) True = (BoolType (x /= y), a ++ [(Diff op)] ++ b)

eval (BoolType x, a) (And op) (BoolType y, b) True = (BoolType (x && y), a ++ [(And op)] ++ b)
eval (BoolType x, a) (Or op) (BoolType y, b) True = (BoolType (x || y), a ++ [(Or op)] ++ b)
eval (BoolType x, a) (Eq op) (BoolType y, b) True = (BoolType (x == y), a ++ [(Eq op)] ++ b)
eval (BoolType x, a) (Diff op) (BoolType y, b) True = (BoolType (x /= y), a ++ [(Diff op)] ++ b)

eval (MatrixInt (l1, c1, m1), a) (PlusMatrix op) (MatrixInt (l2, c2, m2), b) True
  = (MatrixInt (l1, c1, sumMatrix m1 m2), a ++ [(PlusMatrix op)] ++ b)
eval (MatrixDouble (l1, c1, m1), a) (PlusMatrix op) (MatrixDouble (l2, c2, m2), b) True
  = (MatrixDouble (l1, c1, sumMatrix m1 m2), a ++ [(PlusMatrix op)] ++ b)

eval (MatrixInt (l1, c1, m1), a) (MultMatrix op) (MatrixInt (l2, c2, m2), b) True
  = (MatrixInt (l1, c2, mulMatrix m1 m2), a ++ [(MultMatrix op)] ++ b)
eval (MatrixDouble (l1, c1, m1), a) (MultMatrix op) (MatrixDouble (l2, c2, m2), b) True
  = (MatrixDouble (l1, c2, mulMatrix m1 m2), a ++ [(MultMatrix op)] ++ b)

eval (IntType s, a) (Mult op) (MatrixInt (l, c, m), b) True
  = (MatrixInt (l, c, scalarMul s m), a ++ [(Mult op)] ++ b)
eval (DoubleType s, a) (Mult op) (MatrixDouble (l, c, m), b) True
  = (MatrixDouble (l, c, scalarMul s m), a ++ [(Mult op)] ++ b)

eval (IntType s, a) (Plus op) (MatrixInt (l, c, m), b) True
  = (MatrixInt (l, c, scalarSum s m), a ++ [(Plus op)] ++ b)
eval (DoubleType s, a) (Plus op) (MatrixDouble (l, c, m), b) True
  = (MatrixDouble (l, c, scalarMul s m), a ++ [(Plus op)] ++ b)

eval (MatrixInt (l, c, m), a) (Mult op) (IntType s, b) True
  = (MatrixInt (l, c, scalarMul s m), a ++ [(Mult op)] ++ b)
eval (MatrixDouble (l, c, m), a) (Mult op) (DoubleType s, b) True
  = (MatrixDouble (l, c, scalarMul s m), a ++ [(Mult op)] ++ b)

eval (MatrixInt (l, c, m), a) (Plus op) (IntType s, b) True
  = (MatrixInt (l, c, scalarSum s m), a ++ [(Plus op)] ++ b)
eval (MatrixDouble (l, c, m), a) (Plus op) (DoubleType s, b) True
  = (MatrixDouble (l, c, scalarMul s m), a ++ [(Plus op)] ++ b)

-- funções para a tabela de símbolos

get_type :: Token -> CCureState -> Type
get_type _ ([], _, _, _, _, _, _, _) = error "variable not found"
get_type (Id id1 p1) (  (Id id2 _, _, (_, value):tail):t , a, b, c, d, e, f, g) = if id1 == id2 then value
                                             else get_type (Id id1 p1) (t, a, b, c, d, e, f, g)
-- get_type (Id id1 p1) _ = error "o misterio"

get_type_matrix :: Token -> CCureState -> Type

get_type_matrix _ ([], _, _, _, _, _, _, _) = error "variable not found"
get_type_matrix (Id id1 p1) (  (Id id2 _, _, (_, value):tail):t , a, b, c, d, e, f, g) = if id1 == id2 then value
                                             else get_type (Id id1 p1) (t, a, b, c, d, e, f, g)

get_bool_value :: Type -> Bool
get_bool_value (BoolType a) = a
get_bool_value _ = error "token is not a boolean"

symtable_insert :: (Token, String, [(Int, Type)]) -> CCureState -> CCureState
symtable_insert symbol (symt, a, b, c, d, e, f, g)  = (symtable_insert_aux symbol symt, a, b, c, d, e, f, g)

symtable_insert_aux :: (Token, String, [(Int, Type)]) -> SymTable -> SymTable
symtable_insert_aux a [] = [a]
symtable_insert_aux (Id id1 p1, scop1, [value1]) ((Id id2 p2, scop2, value2):tail)
  = if(conseguiInserir) then changedList
    else (Id id1 p1, scop1, [value1]):(Id id2 p2, scop2, value2):tail
    where
      (conseguiInserir, changedList) = insertAux (Id id1 p1, scop1, [value1]) ((Id id2 p2, scop2, value2):tail)

insertAux :: (Token, String, [(Int, Type)]) -> SymTable -> (Bool, SymTable)
insertAux (Id id1 p1, scop1, [value1]) [] = (False, [])
insertAux (Id id1 p1, scop1, [value1]) ((Id id2 p2, scop2, value2):tail)
  = if ((id1,scop1) == (id2, scop2)) then (True, (Id id1 p1, scop1, value1:value2):tail)
    else do
      let (conseguiInserir, changedList) = insertAux (Id id1 p1, scop1, [value1]) tail
      (conseguiInserir, (Id id2 p2, scop2, value2):changedList)


symtable_update_matrix :: (Token, Int, Int, Int, Type) -> CCureState -> CCureState
symtable_update_matrix a (b, c, d, e, f, g, h, i) = (symtable_update_matrix_aux a b, c, d, e, f, g, h, i)

symtable_update_matrix_aux :: (Token, Int, Int, Int, Type) -> SymTable -> SymTable
symtable_update_matrix_aux _ [] = error "variable not found"
symtable_update_matrix_aux (Id id1 p1, depth1, l1, c1, IntType v1) ((Id id2 p2, scop, (depth2, (MatrixInt (l2, c2, m2))):tail):t ) =
  if((id1, depth1) == (id2, depth2)) then (Id id2 p1, scop, (depth2, (MatrixInt (l2, c2, changeMatrix l1 c1 m2 v1))):tail):t
  else (Id id2 p2, scop, (depth2, (MatrixInt (l2, c2, m2))):tail) : symtable_update_matrix_aux (Id id1 p1, depth1, l1, c1, IntType v1) t
symtable_update_matrix_aux (Id id1 p1, depth1, l1, c1, (DoubleType v1)) ((Id id2 p2, scop, (depth2, (MatrixDouble (l2, c2, m2))):tail):t ) =
  if((id1, depth1) == (id2, depth2)) then (Id id2 p1, scop, (depth2, (MatrixDouble (l2, c2, changeMatrix l1 c1 m2 v1))):tail):t
  else (Id id2 p2, scop, (depth2, (MatrixDouble (l2, c2, m2))):tail) : symtable_update_matrix_aux (Id id1 p1, depth1, l1, c1, DoubleType v1) t
symtable_update_matrix_aux (Id id1 p1, depth1, l1, c1, v1) ((Id id2 p2, scop, (depth2, v2):tail):t ) =
  (Id id2 p2, scop, (depth2, v2):tail) : symtable_update_matrix_aux (Id id1 p1, depth1, l1, c1, v1) t
symtable_update_matrix_aux _ _ = error "batata"

symtable_update :: (Token, Int, Type) -> CCureState -> CCureState
symtable_update a (b, c, d, e, f, g, h, i) = (symtable_update_aux a b, c, d, e, f, g, h, i)

symtable_update_aux :: (Token, Int, Type) -> SymTable -> SymTable
symtable_update_aux _ [] = fail "variable not found"
symtable_update_aux (Id id1 p1, depth1, v1) ((Id id2 p2, scop, (depth2, v2):tail):t   ) =
                               if ((id1, depth1) == (id2, depth2)) then (Id id2 p1, scop, (depth2, v1):tail):t
                               else (Id id2 p2, scop, (depth2, v2):tail) : symtable_update_aux (Id id1 p1, depth1, v1) t

symtable_update_fake_many :: [(Token, String, Int)] -> [(Token, String, Int)] -> CCureState -> CCureState
symtable_update_fake_many a b (c, d, e, f, g, h, i, j) = (symtable_update_fake_many_aux a b c, d, e, f, g, h, i, j)

symtable_update_fake_many_aux :: [(Token, String, Int)] -> [(Token, String, Int)] -> SymTable -> SymTable
symtable_update_fake_many_aux [] [] t = t
symtable_update_fake_many_aux (a:as) (b:bs) t = symtable_update_fake_many_aux as bs (symtable_update_fake_aux a b t)

-- symtable_update_fake :: (Token, String, Int) -> (Token, String, Int) -> CCureState -> CCureState
-- symtable_update_fake a b (c, d, e, f, g, h, i, j) = (symtable_update_fake_aux a b c, d, e, f, g, h, i, j)

symtable_update_fake_aux :: (Token, String, Int) -> (Token, String, Int) -> SymTable -> SymTable
symtable_update_fake_aux _ _ [] = fail "variable not found"
symtable_update_fake_aux (Id id1 p1, scop1, depth1) (Id id2 p2, scop2, depth2) ((Id id3 p3, scop3, (depth3, v3):tail):t   ) =
                            if ((id1, scop1, depth1) == (id3, scop3, depth3)) then (Id id2 p2, scop2, (depth2, v3):tail):t
                            else (Id id3 p3, scop3, (depth3, v3):tail) : symtable_update_fake_aux (Id id1 p1, scop1, depth1) (Id id2 p2, scop2, depth2) t


-- Percorre a lista enquanto String for igual a escopo. Depois para
symtable_remove_scope :: String -> Int -> CCureState -> CCureState
symtable_remove_scope a depth (b, c, d, e, f, g, h, i) = (symtable_remove_scope_aux a depth b, c, d, e, f, g, h, i)

symtable_remove_scope_aux :: String -> Int -> SymTable -> SymTable
symtable_remove_scope_aux _ _ [] = []
symtable_remove_scope_aux a depth ((Id id2 p2, scop, [(depth2, v2)]):t   ) =
                            if((a, depth) == (scop, depth2)) then symtable_remove_scope_aux a depth t
                            else (Id id2 p2, scop, [(depth2, v2)]) : t
symtable_remove_scope_aux a depth ((Id id2 p2, scop, (depth2, v2):tail):t   ) =
                            if((a, depth) == (scop, depth2)) then (Id id2 p2, scop, tail) : symtable_remove_scope_aux a depth t
                            else (Id id2 p2, scop, (depth2, v2):tail) : t

-- symtable_remove :: (Token,Token) -> CCureState -> CCureState
-- symtable_remove a (b, c, d, e) = (symtable_remove_aux a b, c, d, e)

-- symtable_remove_aux :: (Token,Token) -> SymTable -> SymTable
-- symtable_remove_aux _ [] = fail "variable not found"
-- symtable_remove_aux (id1, v1) ((id2, v2):t) = 
--                                if id1 == id2 then t
--                                else (id2, v2) : symtable_remove_aux (id1, v1) t

symtable_get :: (Token, Int) -> CCureState -> Maybe Type
symtable_get (a, d) (b, _, _, _, _, _, _, _) = symtable_get_aux (a, d) b

symtable_get_aux :: (Token, Int) -> SymTable -> Maybe Type
symtable_get_aux _ [] = error "variable not found"
symtable_get_aux (Id id1 p1, depth1) ((Id id2 p2, scop, (depth2, v2):tail):t   ) =
                            if ((id1, depth1) == (id2, depth2)) then Just v2
                            else symtable_get_aux (Id id1 p1, depth1) t

symtable_get_info :: Token -> Int -> CCureState -> (Token, String, Int, Type)
symtable_get_info a d (b, _, _, _, _, _, _, _) = symtable_get_info_aux (a, d) b


symtable_get_info_aux :: (Token, Int) -> SymTable -> (Token, String, Int, Type)
symtable_get_info_aux _ [] = error "variable not found"
symtable_get_info_aux (Id id1 p1, depth1) ((Id id2 p2, scop, (depth2, v2):tail):t   ) =
                            if ((id1, depth1) == (id2, depth2)) then (Id id2 p2, scop, depth2, v2)
                            else symtable_get_info_aux (Id id1 p1, depth1) t


symtable_update_reg_attr :: (Token, Int, Token, Type) -> CCureState -> CCureState
symtable_update_reg_attr a (b, c, d, e, f, g, h, i) = (symtable_update_reg_attr_aux a b, c, d, e, f, g, h, i)

symtable_update_reg_attr_aux :: (Token, Int, Token, Type) -> SymTable -> SymTable
symtable_update_reg_attr_aux _ [] = error "variable not found"
symtable_update_reg_attr_aux (Id idTypeToInsert pTypeToInsert, depthTypeToInsert, Id idAttrToupdate pAttrToupdate, vAttrToupdate) ((Id id p, scop, (depth, RegisterType (a, b)):tail):t   ) =
                                if ((idTypeToInsert, depthTypeToInsert) == (id, depth)) then (Id id p, scop, (depth, RegisterType (a, symtable_update_reg_attr_aux_aux (Id idAttrToupdate pAttrToupdate, vAttrToupdate) b)):tail):t
                                else (Id id p, scop, (depth, RegisterType (a, b)):tail) : symtable_update_reg_attr_aux (Id idTypeToInsert pTypeToInsert, depthTypeToInsert, Id idAttrToupdate pAttrToupdate, vAttrToupdate) t
symtable_update_reg_attr_aux (Id idTypeToInsert pTypeToInsert, depthTypeToInsert, Id idAttrToupdate pAttrToupdate, vAttrToupdate) ((Id id p, scop, (depth, v):tail):t   ) =
                                (Id id p, scop, (depth, v):tail) : symtable_update_reg_attr_aux (Id idTypeToInsert pTypeToInsert, depthTypeToInsert, Id idAttrToupdate pAttrToupdate, vAttrToupdate) t

symtable_update_reg_attr_aux_aux :: (Token, Type) -> [(Token, Type)] -> [(Token, Type)]
symtable_update_reg_attr_aux_aux _ [] = error "variable not found"
symtable_update_reg_attr_aux_aux (Id idToInsert pToInsert, vToInsert) ((Id id p, v):t) =
                                if (idToInsert == id) then (Id id p, vToInsert):t
                                else (Id id p, v):symtable_update_reg_attr_aux_aux (Id idToInsert pToInsert, vToInsert) t

isInSymTable :: (Token, Int, String) -> CCureState -> Bool
isInSymTable (a, d, scopToSearch) (b, _, _, _, _, _, _, _) = isInSymTable_aux (a, d, getLastScop scopToSearch) b

isInSymTable_aux :: (Token, Int, String) -> SymTable -> Bool
isInSymTable_aux _ [] = False
isInSymTable_aux (Id id1 p1, depth1, scopToSearch) ((Id id2 p2, scop, (depth2, v2):tail):t   ) =
                            ((id1, depth1, scopToSearch) == (id2, depth2, getLastScop scop)) || isInSymTable_aux (Id id1 p1, depth1, scopToSearch) t

-- invocação do parser para o símbolo de partida 

parser :: [Token] -> IO (Either ParseError [Token])
parser tokens = runParserT program ([], [], [], 0, [], [], [], True) "" tokens

-- main :: IO ()
-- main = case unsafePerformIO (parser (getTokens "problemas/problema4.ccr")) of
--             { Left err -> print err;
--               Right ans -> print "Program ended successfully!"
--             }

main :: IO ()
main = (do  args <- getArgs
            case unsafePerformIO ( parser (getTokens (head args))) of
                      { Left err -> print err;
                        Right ans -> print "Program ended successfully!"
                      }
          )