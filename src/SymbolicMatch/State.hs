module SymbolicMatch.State
    where

import qualified SymbolicMatch.Constr as C
import qualified SymbolicMatch.Env as E
import SymbolicMatch.Expr

data State = State
    {   constr :: !C.ConstrSet
    ,   depth :: !Int
    ,   env :: !E.Env
    ,   gen :: !Int
    ,   maxDepth :: !Int
    } deriving Show

incDepth :: State -> State
incDepth state@State {depth=d} = state{depth=d+1}

newEnv :: State -> E.Env -> State
newEnv state@State {depth=d} e'= state{depth=d+1, env=e'}

init :: E.Env -> Int -> State -- gen must be grater than the original syms in src to avoid collision
init e' maxDepth = State {constr=C.init, depth=0, env=e', gen=1000, maxDepth=maxDepth}

genSym :: State -> (Expr, State)
genSym state@State {gen=g'} = (Sym g', state {gen=g'+1})

genSyms :: Int -> State -> ([Expr], State)
genSyms k state@State {gen=g'} =
    let ids = take k (iterate (+1) g')
        syms = map Sym ids in (syms, state {gen=g'+k})

bind :: Int -> Expr -> State -> State
bind k v state@State {env=e} = state {env=E.bind k v e}

bindAll :: [(Int, Expr)] -> State -> State
bindAll pairs state@State {env=e} = state {env=E.bindAll pairs e}

get :: Int -> State -> Maybe Expr
get k s = E.get k (env s)

unsafeGet :: Int -> State -> String -> Expr
unsafeGet k s = E.unsafeGet k (env s)

assign :: Int -> Expr -> State -> State
assign n exp state@State {constr=cs} = state{constr = C.assign n exp cs}

lookupSym :: Int -> State -> Maybe Expr
lookupSym n State{constr = cs} = C.lookup n cs

appAssign :: Int -> [Expr] -> Expr -> State -> State
appAssign n args exp state@State {constr=cs} = 
    state{constr = C.appAssign n args exp cs}

lookupApp :: Int -> [Expr] -> State -> Maybe Expr
lookupApp n args State{constr = cs} = C.lookupApp n args cs

getAssign :: Int -> State -> Maybe Expr
getAssign s state = C.getAssign s (constr state)

buildFromConstr :: State -> Expr -> Expr
buildFromConstr state = C.buildFromConstr (constr state)

reachMaxDepth :: State -> Bool
reachMaxDepth State {depth=d, maxDepth=m} = d > m