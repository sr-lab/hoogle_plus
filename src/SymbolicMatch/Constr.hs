{-# LANGUAGE LambdaCase #-}
module SymbolicMatch.Constr
    ( ConstrSet, SymbolicMatch.Constr.init, assign, appAssign, getAssign, buildFromConstr, pretty, prettyAll
    ) where

import SymbolicMatch.Expr ( Expr(Lit, Sym, DataC, WildCard), symbols )
import Data.List ( sort )
import Data.Maybe ( catMaybes )
import qualified Data.IntMap as M
import qualified Data.Map as M2

data ConstrSet = ConstrSet { symAsgns :: M.IntMap Expr
                           , appAsgns :: M2.Map (Int, [Expr]) Expr}
                           deriving Show

init :: ConstrSet
init = ConstrSet {symAsgns = M.empty, appAsgns = M2.empty}

assign :: Int -> Expr -> ConstrSet -> Maybe ConstrSet
assign n e cs@ConstrSet{symAsgns=sa, appAsgns=aa} =
  case M.lookup n sa of
    Nothing -> Just ConstrSet {symAsgns = M.insert n e sa, appAsgns = aa}
    Just e' -> matchable e e' cs


appAssign :: Int -> [Expr] -> Expr -> ConstrSet -> Maybe ConstrSet
appAssign n args e cs@ConstrSet{symAsgns=sa, appAsgns=aa} =
    case M2.lookup (n, args) aa of
      Nothing -> Just ConstrSet {symAsgns = sa, appAsgns = M2.insert (n, args) e aa}
      Just e' -> matchable e e' cs


getAssign :: Int -> ConstrSet -> Maybe Expr
getAssign n cs = M.lookup n (symAsgns cs)

-- transform l = [1, $e], $e = 5 into l = [1, 5]
buildFromConstr :: ConstrSet -> Expr -> Expr
buildFromConstr cs@ConstrSet{symAsgns=sa, appAsgns=aa} e = case e of
  Sym s -> case getAssign s cs of
    Just e' -> buildFromConstr cs e'
    Nothing
      | symIsArgInConstr s -> Sym s
      | otherwise -> WildCard
  DataC s exs -> DataC s (map (buildFromConstr cs) exs)
  Lit lit -> e
  WildCard -> WildCard
  _ -> error $ "not a final expression: " ++  show e
  where
    symIsArgInConstr :: Int -> Bool
    symIsArgInConstr s = M2.foldrWithKey (\(_, es) _ r -> any (elem s . symbols) es || r) False aa

pretty :: Expr -> ConstrSet -> [(Int, [Expr], Expr)]
pretty e cs@ConstrSet{symAsgns=sa, appAsgns=aa} = sort $ catMaybes $
    map prettySymAsgn (M.assocs sa)
    ++
    map prettyAppAsgn (M2.assocs aa)
  where
    syms :: [Int]
    syms = symbols e

    prettySymAsgn :: (Int, Expr) -> Maybe (Int, [Expr], Expr)
    prettySymAsgn (name, e')
      | name `elem` syms = Just (name, [], buildFromConstr cs e')
      | otherwise = Nothing

    prettyAppAsgn :: ((Int, [Expr]), Expr) -> Maybe (Int, [Expr], Expr)
    prettyAppAsgn ((name, es'), e')
      | name `elem` syms = Just (name, es', buildFromConstr cs e')
      | otherwise = Nothing

-- presents all symbols, not only the one present on a specified expression
prettyAll :: ConstrSet -> [(Int, [Expr], Expr)]
prettyAll cs@ConstrSet{symAsgns=sa, appAsgns=aa} = sort $ catMaybes $
    map prettySymAsgn (M.assocs sa)
    ++
    map prettyAppAsgn (M2.assocs aa)
  where
    prettySymAsgn :: (Int, Expr) -> Maybe (Int, [Expr], Expr)
    prettySymAsgn (name, e') = Just (name, [], buildFromConstr cs e')

    prettyAppAsgn :: ((Int, [Expr]), Expr) -> Maybe (Int, [Expr], Expr)
    prettyAppAsgn ((name, es'), e') = Just (name, es', buildFromConstr cs e')

-- a weaker version of match for final version expressions
-- FIXME think about using match instead of this
-- or using a proper AST for reduced expressions
-- no backtrack, no choices. Only one solution if can, so no ct.
matchable :: Expr -> Expr -> ConstrSet -> Maybe ConstrSet
matchable (Sym n) (Sym n') cs'@ConstrSet{symAsgns=sa, appAsgns=aa}
  | n == n' = Just cs'
  | otherwise = case getAssign n cs' of
      Nothing -> Just $ ConstrSet {symAsgns = M.insert n (Sym n') sa, appAsgns = aa}
      Just e -> case getAssign n' cs' of
        Nothing -> Just $ ConstrSet {symAsgns = M.insert n' (Sym n) sa, appAsgns = aa}
        Just e' -> error "For now, we only assign symbols in which one is fresh"

matchable (Sym n) e2' cs'@ConstrSet{symAsgns=sa, appAsgns=aa} =
  case getAssign n cs' of
    Nothing -> Just $ ConstrSet {symAsgns = M.insert n e2' sa, appAsgns = aa}
    Just e -> matchable e e2' cs'

matchable e1' (Sym n) cs'@ConstrSet{symAsgns=sa, appAsgns=aa} =
  case getAssign n cs' of
    Nothing -> Just $ ConstrSet {symAsgns = M.insert n e1' sa, appAsgns = aa}
    Just e -> matchable e e1' cs'

matchable (Lit l) (Lit l') cs'
  | l == l' = Just cs'
  | otherwise = Nothing

matchable (DataC n es) (DataC n' es') cs'
  | n == n' = matchablePairs es es' cs'
  | otherwise = Nothing
  where
    matchablePairs :: [Expr] -> [Expr] -> ConstrSet -> Maybe ConstrSet
    matchablePairs [] [] cs' = Just cs'
    matchablePairs (e:es) (e':es') cs' = case matchable e e' cs' of
      Nothing -> Nothing
      Just cs'' -> matchablePairs es es' cs''
    matchablePairs _ _ _ = Nothing

matchable _ _ _ = Nothing