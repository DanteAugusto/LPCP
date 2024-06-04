module Main (main) where

import Lexer
import Text.Parsec
import Control.Monad.IO.Class

import System.IO.Unsafe

-- parsers para os tokens

programToken = tokenPrim show update_pos get_token where
  get_token (Program p) = Just (Program p)
  get_token _           = Nothing

endToken = tokenPrim show update_pos get_token where
  get_token (End p) = Just (End p)
  get_token _           = Nothing

idToken = tokenPrim show update_pos get_token where
  get_token (Id x p) = Just (Id x p)
  get_token _        = Nothing

semiColonToken = tokenPrim show update_pos get_token where
  get_token (Semicolon p) = Just (Semicolon p)
  get_token _             = Nothing

assignToken = tokenPrim show update_pos get_token where
  get_token (Assign p) = Just (Assign p)
  get_token _          = Nothing

intToken = tokenPrim show update_pos get_token where
  get_token (Int p) = Just (Int p)
  get_token _         = Nothing

intLiteralToken :: ParsecT [Token] st IO (Token)
intLiteralToken = tokenPrim show update_pos get_token where
  get_token (IntLit x p) = Just (IntLit x p)
  get_token _            = Nothing

plusToken = tokenPrim show update_pos get_token where
  get_token (Plus p) = Just (Plus p)
  get_token _       = Nothing 

update_pos :: SourcePos -> Token -> [Token] -> SourcePos
update_pos pos _ (tok:_) = pos -- necessita melhoria
update_pos pos _ []      = pos  


-- typeDeclarationsToken = tokenPrim show update_pos get_token where
--   get_token TypeDeclarations = Just TypeDeclarations
--   get_token _       = Nothing

-- endTypeDeclarationsToken = tokenPrim show update_pos get_token where
--   get_token EndTypeDeclarations = Just EndTypeDeclarations
--   get_token _       = Nothing

-- typeToken = tokenPrim show update_pos get_token where
--   get_token Type = Just Type
--   get_token _       = Nothing

-- endTypeToken = tokenPrim show update_pos get_token where
--   get_token EndType = Just EndType
--   get_token _       = Nothing

-- globalVariablesToken = tokenPrim show update_pos get_token where
--   get_token GlobalVariables = Just GlobalVariables
--   get_token _       = Nothing
  
-- endGlobalVariablesToken = tokenPrim show update_pos get_token where
--   get_token EndGlobalVariables = Just EndGlobalVariables
--   get_token _       = Nothing

-- subprogramsToken = tokenPrim show update_pos get_token where
--   get_token Subprograms = Just Subprograms
--   get_token _       = Nothing

-- endSubprogramsToken = tokenPrim show update_pos get_token where
--   get_token EndSubprograms = Just EndSubprograms
--   get_token _       = Nothing

-- programToken = tokenPrim show update_pos get_token where
--   get_token Program = Just Program
--   get_token _       = Nothing

-- endToken = tokenPrim show update_pos get_token where
--   get_token End = Just End
--   get_token _       = Nothing

-- funToken = tokenPrim show update_pos get_token where
--   get_token Fun = Just Fun
--   get_token _       = Nothing

-- endFunToken = tokenPrim show update_pos get_token where
--   get_token EndFun = Just EndFun
--   get_token _       = Nothing

-- returnToken = tokenPrim show update_pos get_token where
--   get_token Return = Just Return
--   get_token _       = Nothing

-- semicolonToken = tokenPrim show update_pos get_token where
--   get_token Semicolon = Just Semicolon
--   get_token _       = Nothing

-- arrowToken = tokenPrim show update_pos get_token where
--   get_token Arrow = Just Arrow
--   get_token _       = Nothing

-- plusToken = tokenPrim show update_pos get_token where
--   get_token Plus = Just Plus
--   get_token _       = Nothing

-- openParentToken = tokenPrim show update_pos get_token where
--   get_token OpenParent = Just OpenParent
--   get_token _       = Nothing

-- closeParentToken = tokenPrim show update_pos get_token where
--   get_token CloseParent = Just CloseParent
--   get_token _       = Nothing

-- commaToken = tokenPrim show update_pos get_token where
--   get_token Comma = Just Comma
--   get_token _       = Nothing

-- intToken = tokenPrim show update_pos get_token where
--   get_token Int = Just Int
--   get_token _       = Nothing

-- doubleToken = tokenPrim show update_pos get_token where
--   get_token Double = Just Double
--   get_token _       = Nothing

-- intLitToken = tokenPrim show update_pos get_token where
--   get_token (IntLit x) = Just (IntLit x)
--   get_token _      = Nothing

-- doubleLitToken = tokenPrim show update_pos get_token where
--   get_token (DoubleLit x) = Just (DoubleLit x)
--   get_token _      = Nothing

-- idToken = tokenPrim show update_pos get_token where
--   get_token (Id x) = Just (Id x)
--   get_token _      = Nothing

-- typeidToken = tokenPrim show update_pos get_token where
--   get_token (TypeId x) = Just (TypeId x)
--   get_token _      = Nothing

-- parsers para os não-terminais

program :: ParsecT [Token] [(Token,Token)] IO ([Token])
program = do
            a <- programToken 
            b <- stmts
            c <- endToken
            eof
            return ([a] ++ b ++ [c])

varDecl :: ParsecT [Token] [(Token,Token)] IO([Token])
varDecl = do
            a <- intToken
            b <- assignToken
            c <- expression
            updateState(symtable_insert (a, c))
            s <- getState
            liftIO (print s)
            return (a:b:[c])

stmts :: ParsecT [Token] [(Token,Token)] IO([Token])
stmts = do
          first <- varDecl
          next <- remainingStmts
          return (first ++ next)

assign :: ParsecT [Token] [(Token,Token)] IO([Token])
assign = do
          a <- idToken
          b <- assignToken
          c <- expression
          d <- semiColonToken
          s <- getState
          if (not (compatible (get_type a s) c)) then fail "type mismatch"
          else 
            do 
              updateState(symtable_update (a, c))
              s <- getState
              liftIO (print s)
              return (a:b:c:[d])

get_type :: Token -> [(Token, Token)] -> Token
get_type _ [] = error "variable not found"
get_type (Id id1 p1) ((Id id2 _, value):t) = if id1 == id2 then value
                                             else get_type (Id id1 p1) t

expression :: ParsecT [Token] [(Token,Token)] IO(Token)
expression = try bin_expression <|> una_expression

una_expression :: ParsecT [Token] [(Token,Token)] IO(Token)
una_expression = do
                   op <- plusToken
                   a <- intToken 
                   return (a)
   
--- funções considerando associatividade à esquerda                  
bin_expression :: ParsecT [Token] [(Token,Token)] IO(Token)
bin_expression = do
                   n1 <- intToken
                   result <- eval_remaining n1
                   return (result)

eval_remaining :: Token -> ParsecT [Token] [(Token,Token)] IO(Token)
eval_remaining n1 = do
                      op <- plusToken
                      n2 <- intToken
                      result <- eval_remaining (eval n1 op n2)
                      return (result) 
                    <|> return (n1)                              

eval :: Token -> Token -> Token -> Token
eval (IntLit x p) (Plus _ ) (IntLit y _) = IntLit (x + y) p

remainingStmts :: ParsecT [Token] [(Token,Token)] IO([Token])
remainingStmts = (do a <- assign
                     return a) <|> (return [])

compatible :: Token -> Token -> Bool
compatible (IntLit _ _) (IntLit _ _) = True
compatible _ _ = False

-- funções para a tabela de símbolos

symtable_insert :: (Token,Token) -> [(Token,Token)] -> [(Token,Token)]
symtable_insert symbol []  = [symbol]
symtable_insert symbol symtable = symtable ++ [symbol]

symtable_update :: (Token,Token) -> [(Token,Token)] -> [(Token,Token)]
symtable_update _ [] = fail "variable not found"
symtable_update (id1, v1) ((id2, v2):t) = 
                               if id1 == id2 then (id1, v1) : t
                               else (id2, v2) : symtable_update (id1, v1) t

symtable_remove :: (Token,Token) -> [(Token,Token)] -> [(Token,Token)]
symtable_remove _ [] = fail "variable not found"
symtable_remove (id1, v1) ((id2, v2):t) = 
                               if id1 == id2 then t
                               else (id2, v2) : symtable_remove (id1, v1) t                               


-- invocação do parser para o símbolo de partida 

parser :: [Token] -> IO (Either ParseError [Token])
parser tokens = runParserT program [] "Error message" tokens

main :: IO ()
main = case unsafePerformIO (parser (getTokens "soma.ccr")) of
            { Left err -> print err; 
              Right ans -> print ans
            }
