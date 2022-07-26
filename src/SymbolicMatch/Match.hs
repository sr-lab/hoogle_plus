{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}


module SymbolicMatch.Match
    ( matchExprs, matchExprsPretty, MatchError(..))
     where

import Data.List ( sort )
import Data.Maybe ( mapMaybe, catMaybes )
import SymbolicMatch.Expr ( Expr(..), Alt(..), final)
import qualified SymbolicMatch.Constr as C
import qualified SymbolicMatch.State as S
import qualified SymbolicMatch.Env as E
import SymbolicMatch.Eval ( eval )
import SymbolicMatch.Constr (pretty)

data MatchError = DepthReached
                | Mismatch
                | Exception String
                deriving (Show, Eq)

match
    :: Expr
    -> S.State
    -> Expr
    -> (S.State -> Either MatchError C.ConstrSet)
    -> Either MatchError C.ConstrSet

match _ state WildCard ct
  | S.reachMaxDepth state = Left DepthReached
  | otherwise = ct (S.incDepth state)

match WildCard state _ ct = Left $ Exception "Src should not be WildCard"

match (Lam n e) state dst ct = Left $ Exception "for now, do not match lambdas!"

match (Lit l) state e2 ct
  | S.reachMaxDepth state = Left DepthReached
  | otherwise =
      case e2 of
        (Lit l') ->
          if l == l'
            then ct (S.incDepth state)
            else Left Mismatch
        (Sym s) -> case S.assign s (Lit l) state of
          Nothing -> Left Mismatch
          Just st -> ct (S.incDepth st)
        _ -> Left Mismatch

match (Var n) state dst ct
  | S.reachMaxDepth state = Left DepthReached
  | otherwise =
      case S.get n state of
        Nothing -> Left $ Exception $ "Match variable " ++ show n ++ " not found"
        Just e -> match e (S.incDepth state) dst ct

-- sometimes the dst can have symbols
-- example: match scrutinee, match app
-- try match the sym in dst first than the sym in src
-- FIXME if fail, try the other?
-- sera mesmo necessario? é que com o WildCard, que simbolos é que aparecem em dst?
match src state (Sym n) ct
  | S.reachMaxDepth state = Left DepthReached
  | otherwise =
      case S.assign n src state of
        Just state' -> ct (S.incDepth state')
        Nothing -> Left Mismatch

match (Sym n) state dst ct
  | S.reachMaxDepth state = Left DepthReached
  | otherwise =
      case S.assign n dst state of
        Just state' -> ct (S.incDepth state')
        Nothing -> Left Mismatch

match (DataC n es) state dst ct
  | S.reachMaxDepth state = Left DepthReached
  | otherwise =
      case dst of
        DataC n' es' ->
            if n == n' && length es == length es'
                then matchPairs (zip es es') (S.incDepth state) ct
                else Left Mismatch
        _ -> Left Mismatch

match (Case e alts) state dst ct
  | S.reachMaxDepth state = Left DepthReached
  | otherwise = matchScrutinee e (Alt "Error" [] (DataC "Error" []):alts) state dst ct
  where
    -- match the case scrutinee to the patterns
    -- and then match the alternative to the dst
    matchScrutinee :: Expr -> [Alt] -> S.State -> Expr -> (S.State -> Either MatchError C.ConstrSet) -> Either MatchError C.ConstrSet
    matchScrutinee _ [] _ _ _ = Left Mismatch
    matchScrutinee scr ((Alt s ss e):alts) state dst ct =
      let (syms, state') = S.genSyms (length ss) state
          ct' = \st -> match e (S.bindAll (zip ss syms) st) dst (\st' -> ct (S.newEnv st' (S.env st))) in
        case match scr state' (DataC s syms) ct' of
          Right cs' -> Right cs'
          Left err -> matchScrutinee scr alts state dst ct


match (App e es) state dst ct
  | S.reachMaxDepth state = Left DepthReached
  | otherwise =
      case e of
        (Lam ps e') ->
          if length es == length ps
            then let es' = map (eval state) es in
              if all final es'
                then let state' = S.bindAll (zip ps es') state
                         ct' = \st -> ct (S.newEnv st (S.env state)) in -- FIXME and what about remove the generated symbols
                    match e' (S.incDepth state') dst ct'
                else let (syms, state') = S.genSyms (length es') state
                         ct' = \st -> matchApp es syms (S.newEnv st (S.env state)) ct in
                    match (App e syms) state' dst ct'
            else Left $ Exception $ "argument and param mismatch: " ++ show e ++ ";;;" ++ show es
        (Var n) ->
          let e' = S.unsafeGet n state ("App variable " ++ show n ++ " not found") in
            match (App e' es) (S.incDepth state) dst ct
        (Sym n) -> -- expects that the arguments of symbols applications does not have branches
          let es' = map (eval state) es in
              case S.appAssign n es' dst state of
                Just state' -> ct state'
                Nothing -> Left Mismatch
                
        a -> Left $ Exception $ "App expression not a var nor lam" ++ show a
  where
    matchApp :: [Expr]
        -> [Expr]
        -> S.State
        -> (S.State -> Either MatchError C.ConstrSet)
        -> Either MatchError C.ConstrSet
    matchApp args syms state ct
      | S.reachMaxDepth state = Left DepthReached
      | otherwise = matchPairs (zip args exprs) (S.incDepth state) ct
      where
        exprs = map (S.buildFromConstr state) syms
        -- FIXME o que acontece se a expr não tiver restrições, WildCard?


matchPairs :: [(Expr, Expr)]
  -> S.State
  -> (S.State -> Either MatchError C.ConstrSet)
  -> Either MatchError C.ConstrSet
matchPairs [] state ct
  | S.reachMaxDepth state = Left DepthReached
  | otherwise = ct (S.incDepth state)

matchPairs ((e1, e2):es) state ct
  | S.reachMaxDepth state = Left DepthReached
  | otherwise =
      let ct' = \st -> matchPairs es (S.incDepth st) ct in
        match e1 (S.incDepth state) e2 ct'

idCont :: S.State -> Either MatchError C.ConstrSet
idCont state = Right $ S.constr state

-- entry points
matchExprs :: Int -> Expr -> E.Env ->  Expr -> Either MatchError C.ConstrSet
matchExprs depth e1 env dst =
  match e1 (S.init env depth) dst idCont

matchExprsPretty :: Int -> Expr -> E.Env ->  Expr -> Either MatchError [(Int, [Expr], Expr)]
matchExprsPretty depth e1 env dst = 
  match e1 (S.init env depth) dst idCont >>= \cs -> Right $ pretty e1 cs