{-# LANGUAGE LambdaCase #-}
module SymbolicMatch.Eval (eval, choosePoly, evalMany) where

import qualified SymbolicMatch.State as S
import SymbolicMatch.Expr

-- evalutes everything possible without symbolic branches
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

eval _ lit@(Lit _) = Right lit

eval st (App e es) =
  case eval st e of
    Right (Lam ps exp) -> do
      es' <- evalMany st es
      let st' = S.bindAll (zip ps es') st
      eval st' exp
    Right (Poly table) -> do
      es' <- evalMany st es
      let lam = choosePoly table es'
      eval st (App lam es')        
    Right o -> do
      es' <- evalMany st es
      Right $ App o es'
    Left err -> Left err

eval st (Case e alts) =
  case eval st e of
    Right (DataC "Error" []) -> Right $ DataC "Error" []
    Right (DataC s exs) -> searchAlt s exs alts
    Right o -> Right $ Case o alts
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
evalMany st (e:es) = case eval st e of
  Left s -> Left s
  Right ex -> case evalMany st es of
    Left s -> Left s
    Right exs -> Right (ex:exs)

-- given a poly table and the arguments, choose lambda
-- arguments must be evaluated!
choosePoly :: PolyTable -> [Expr] -> Expr
choosePoly [] es = error $ "No suitable alternatives for poly: " ++ show es
choosePoly (PolyAlt rules lam:alts) args
  | all ruleSat rules = lam
  | otherwise =  choosePoly alts args
  where
    ruleSat :: Rule -> Bool
    ruleSat (DataConsIn argInd datacs)
      | argInd < length args = 
        case args !! argInd of
          DataC n _-> n `elem` datacs
          _ -> False
      | otherwise = error "Bad rule"