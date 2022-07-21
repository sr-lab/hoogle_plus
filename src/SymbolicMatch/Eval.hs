{-# LANGUAGE LambdaCase #-}
module SymbolicMatch.Eval (eval) where

import qualified SymbolicMatch.State as S
import SymbolicMatch.Expr

-- evalutes everything possible without symbolic branches
eval :: S.State -> Expr -> Expr

eval st sym@(Sym n) = case S.getAssign n st of
  Nothing -> sym
  Just ex -> eval st ex

eval _ lam@(Lam _ _) = lam

eval st (Var n) = eval st $ S.unsafeGet n st $ "Variable " ++ show n ++ " not found."

eval st (DataC n args) = DataC n (map (eval st) args)

eval _ lit@(Lit _) = lit

eval st (App e es) = 
  case eval st e of
    (Lam ps exp) -> 
      let es' = map (eval st) es
          st' = S.bindAll (zip ps es') st in
        eval st' exp
    o -> App o $ map (eval st) es

eval st (Case e alts) =
  case eval st e of
    (DataC "Error" []) -> DataC "Error" []
    (DataC s exs) -> searchAlt s exs alts
    o -> Case o alts
  where 
    searchAlt :: String -> [Expr] -> [Alt] -> Expr
    searchAlt n es [] = error $ "No alternative found - " ++ n ++ show es ++ "; " ++ show alts
    searchAlt n es ((Alt s ss body):t)
      | n == s && length es == length ss = 
        let st' = S.bindAll (zip ss es) st in
          eval st' body
      | otherwise = searchAlt n es t


eval _ w@WildCard = w