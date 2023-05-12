{-# LANGUAGE LambdaCase #-}
module SymbolicMatch.Eval (eval, evalMany) where

import qualified SymbolicMatch.State as S
import SymbolicMatch.Expr
import Debug.Trace

-- evalutes everything possible without symbolic branches
-- with the exception of App and Case, because it is used in 
-- App case of match, and, due to shadowing, the variables are no
-- corectly replaced, so leave this work to match
-- to prevent infinite eval'uation, there is a max depth d
eval :: Int -> S.State -> Expr -> Either String Expr
eval d st e = case eval' d st e of
  Left s -> Left s
  Right Nothing -> Right e -- taken all the fuel, probably an infinite expressions
  Right (Just e') -> Right e'

eval' :: Int -> S.State -> Expr -> Either String (Maybe Expr) -- Nothing means depth reached
eval' d st sym@(Sym n) 
  | d <= 0 = Right Nothing
  | otherwise = case S.getAssign n st of
      Nothing -> Right (Just sym)
      Just ex -> eval' (d-1) st ex

eval' _ _ lam@(Lam _ _) = Right (Just lam)

eval' _ st poly@(Poly _) = Right (Just poly)

eval' d st (Var n) = case S.get n st of
  Nothing -> Left $ "Variable " ++ show n ++ " not found."
  Just v -> eval' d st v

eval' d st (DataC n args) = case evalMany' d st args of
  Left err -> Left err
  Right Nothing -> Right Nothing
  Right (Just args) -> Right $ Just $ DataC n args

eval' d st (App e es) 
  | d <= 0 = Right Nothing
  | otherwise =
      case eval' d st e of
        Right (Just (Lam ps exp)) -> do
          es' <- evalMany' (d-1) st es
          case es' of 
            Nothing -> Right Nothing
            Just es'' -> do 
              let st' = S.bindAll (zip ps es'') st
              eval' (d-1) st' exp
        Right (Just (Poly lambs)) -> do
          es' <- evalMany' d st es
          case es' of
            Nothing -> Right Nothing
            Just es'' -> tryLams (d-2) lambs es''
          where
            tryLams :: Int -> [Expr] -> [Expr] -> Either String (Maybe Expr)
            tryLams _ [] args = Left "eval': no lambda for poly"
            tryLams d (h:t) args = case eval' d st (App h args) of 
                Left s -> tryLams (d-1) t args
                Right ex -> Right ex

        Right (Just o) -> do
          es' <- evalMany' d st es
          case es' of
            Nothing -> Right Nothing
            Just es'' -> Right $ Just (App o es'')
        Left err -> Left err

        Right Nothing -> Right Nothing

eval' d st (Case e alts) =
  case eval' d st e of
    Right (Just (DataC "Error" [])) -> Right $ Just $ DataC "Error" []
    Right (Just (DataC s exs)) -> searchAlt (d-1) s exs alts
    Right (Just o) -> Right $ Just (Case o alts)
    Right Nothing -> Right Nothing
    Left err -> Left err
  where 
    searchAlt :: Int -> String -> [Expr] -> [Alt] -> Either String (Maybe Expr)
    searchAlt _ n es [] = Left $ "No alternative found - " ++ n ++ show es ++ "; " ++ show alts
    searchAlt d n es ((Alt s ss body):t)
      | n == s && length es == length ss = 
        let st' = S.bindAll (zip ss es) st in
          eval' d st' body
      | otherwise = searchAlt (d-1) n es t

eval' _ _ w@WildCard = Right (Just w)

evalMany' :: Int -> S.State -> [Expr] -> Either String (Maybe [Expr]) -- Nothing means one of the expressions cosumed all the fuel
evalMany' _ st [] = Right (Just [])
evalMany' d st (e:es) = do
  e' <- eval' d st e
  case e' of
    Nothing -> return Nothing
    Just e'' -> do
      es' <- evalMany' (d-1) st es 
      case es' of
        Nothing -> return Nothing
        Just es'' -> return $ Just (e'':es'')

evalMany :: Int -> S.State -> [Expr] -> Either String [Expr]
evalMany d st es = case evalMany' d st es of
  Left ex -> Left ex 
  Right Nothing -> Right es
  Right (Just es') -> Right es'