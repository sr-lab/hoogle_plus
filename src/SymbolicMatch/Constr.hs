module SymbolicMatch.Constr
    ( ConstrSet, SymbolicMatch.Constr.init, assign, SymbolicMatch.Constr.lookup, lookupApp, appAssign, getAssign, buildFromConstr, pretty, prettyAll
    ) where

import SymbolicMatch.Expr ( Expr(Sym, DataC, WildCard), symbols )
import Data.List ( sort )
import Data.Maybe ( catMaybes )
import qualified Data.IntMap as M
import qualified Data.Map as M2

data ConstrSet = ConstrSet { symAsgns :: !(M.IntMap Expr)
                           , appAsgns :: !(M2.Map (Int, [Expr]) Expr)}
                           deriving Show

init :: ConstrSet
init = ConstrSet {symAsgns = M.empty, appAsgns = M2.empty}

assign :: Int -> Expr -> ConstrSet -> ConstrSet
assign n e cs@ConstrSet{symAsgns=sa} = cs {symAsgns = M.insert n e sa}

lookup :: Int -> ConstrSet -> Maybe Expr
lookup n ConstrSet{symAsgns = sa} = M.lookup n sa

lookupApp :: Int -> [Expr] -> ConstrSet -> Maybe Expr
lookupApp n args ConstrSet{appAsgns = aa} = M2.lookup (n, args) aa

appAssign :: Int -> [Expr] -> Expr -> ConstrSet -> ConstrSet
appAssign n es e cs@ConstrSet{appAsgns=aa} = cs{appAsgns = M2.insert (n,es) e aa}

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

-- presents all symbols, not only the ones present on a specified expression
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