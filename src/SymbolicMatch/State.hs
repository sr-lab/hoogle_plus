module SymbolicMatch.State
    where

import qualified SymbolicMatch.Constr as C
import qualified SymbolicMatch.Env as E
import SymbolicMatch.Expr

data State = State
    {   constr :: C.ConstrSet
    ,   depth :: Int
    ,   env :: E.Env
    ,   gen :: Int
    ,   maxDepth :: Int
    }

-- FIXME varias n sao necessarias
-- FIXME [Constr] -> C.Set
incDepth :: State -> State
incDepth State {constr=c, depth=d, env=e, gen=g, maxDepth=m} =
    State {constr=c, depth=d+1, env=e, gen=g, maxDepth=m}

newConstr :: State -> C.ConstrSet -> State
newConstr State {constr=c, depth=d, env=e, gen=g, maxDepth=m} c'=
    State {constr=c', depth=d+1, env=e, gen=g, maxDepth=m}

newEnv :: State -> E.Env -> State
newEnv State {constr=c, depth=d, env=e, gen=g, maxDepth=m} e'=
    State {constr=c, depth=d+1, env=e', gen=g, maxDepth=m}

newState :: State -> C.ConstrSet -> E.Env -> State
newState State {constr=_, depth=d, env=_, gen=g', maxDepth=m} c' e'=
    State {constr=c', depth=d+1, env=e', gen=g', maxDepth=m}


init :: E.Env -> Int -> State -- gen must be grater than the original syms to avoid collision
init e' maxDepth = State {constr=C.init, depth=0, env=e', gen=1000, maxDepth=maxDepth}


genSym :: State -> (Expr, State)
genSym State {constr=c', depth=d, env=e', gen=g', maxDepth=m} =
    (Sym g', State {constr=c', depth=d+1, env=e', gen=g'+1, maxDepth=m})


genSyms :: Int -> State -> ([Expr], State)
genSyms k State {constr=c', depth=d, env=e', gen=g', maxDepth=m} =
    let ids = take k (iterate (+1) g')
        syms = map Sym ids in
            (syms, State {constr=c', depth=d, env=e', gen=g'+k, maxDepth=m})


bind :: Int -> Expr -> State -> State
bind k v State {constr=c, depth=d, env=e, gen=g, maxDepth=m} =
    State {constr=c, depth=d, env=E.bind k v e, gen=g, maxDepth=m}


bindAll :: [(Int, Expr)] -> State -> State
bindAll pairs State {constr=c, depth=d, env=e, gen=g, maxDepth=m} =
    State {constr=c, depth=d, env=E.bindAll pairs e, gen=g, maxDepth=m}


get :: Int -> State -> Maybe Expr
get k s = E.get k (env s)


unsafeGet :: Int -> State -> String -> Expr
unsafeGet k s = E.unsafeGet k (env s)


assign :: Int -> Expr -> State -> Maybe State
assign n exp state@State {constr=cs, depth=d, env=e, gen=g, maxDepth=m} =
  case C.assign n exp cs of
    Just cs' -> Just State {constr=cs', depth=d, env=e, gen=g, maxDepth=m}
    Nothing -> Nothing

appAssign :: Int -> [Expr] -> Expr -> State -> Maybe State
appAssign n args exp state@State {constr=cs, depth=d, env=e, gen=g, maxDepth=m} =
  case C.appAssign n args exp cs of
    Just cs' -> Just State {constr=cs', depth=d, env=e, gen=g, maxDepth=m}
    Nothing -> Nothing

getAssign :: Int -> State -> Maybe Expr
getAssign s state = C.getAssign s (constr state)

buildFromConstr :: State -> Expr -> Expr
buildFromConstr state = C.buildFromConstr (constr state)

reachMaxDepth :: State -> Bool
reachMaxDepth State {depth=d, maxDepth=m} = d > m