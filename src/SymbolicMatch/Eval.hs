{-# LANGUAGE LambdaCase #-}
module SymbolicMatch.Eval (eval, evalMany) where

import qualified SymbolicMatch.State as S
import SymbolicMatch.Expr

-- evalutes everything possible without symbolic branches
-- with the exception of App and Case, because it is used in 
-- App case of match, and, due to shadowing, the variables are no
-- corectly replaced, so leave this work to match
eval :: S.State -> Expr -> Either String Expr

eval st sym@(Sym n) = case S.getAssign n st of
  Nothing -> Right sym
  Just ex -> eval st ex

eval _ lam@(Lam _ _) = Right lam

eval st poly@(Poly _) = Right poly

eval st (Var n) = case S.get n st of
  Nothing -> Left $ "Variable " ++ show n ++ " not found."
  Just v -> eval st v

eval st (DataC n args) = case evalMany st args of
  Left err -> Left err
  Right args -> Right $ DataC n args

eval st (App e es) =
  case eval st e of
    Right (Lam ps exp) -> do
      es' <- evalMany st es
      --return $ App (Lam ps exp) es'
      let st' = S.bindAll (zip ps es') st
      --return $ 
      eval st' exp
    Right (Poly lambs) -> do
      es' <- evalMany st es
      tryLams lambs es'      
      where
        tryLams :: [Expr] -> [Expr] -> Either String Expr
        tryLams [] args = Left "eval: no lambda for poly"
        tryLams (h:t) args = case eval st (App h args) of 
            Left s -> tryLams t args
            Right ex -> Right ex

    Right o -> do
      es' <- evalMany st es
      Right $ App o es'
    Left err -> Left err

eval st (Case e alts) =
  case eval st e of
    Right (DataC "Error" []) -> Right $ DataC "Error" []
    Right (DataC s exs) -> searchAlt s exs alts
    Right o -> Right $ Case o alts -- não se pode avaliar as alts con 
    --shadowing do env. ex.: Lam [0, 1] (Case () -- Cons 2 3) => pode substituir 2 e 3 se já existir
    Left err -> Left err
  where 
    searchAlt :: String -> [Expr] -> [Alt] -> Either String Expr
    searchAlt n es [] = Left $ "No alternative found - " ++ n ++ show es ++ "; " ++ show alts
    searchAlt n es ((Alt s ss body):t)
      | n == s && length es == length ss = 
        let st' = S.bindAll (zip ss es) st in
          eval st' body
      | otherwise = searchAlt n es t

eval _ w@WildCard = Right w

evalMany :: S.State -> [Expr] -> Either String [Expr]
evalMany st [] = Right []
evalMany st (e:es) = do
  e' <- eval st e
  es' <- evalMany st es 
  return (e':es')