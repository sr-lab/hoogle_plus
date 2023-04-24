module SymbolicMatch.Env
    ( Env, SymbolicMatch.Env.init, bind, bindAll, get, unsafeGet
    ) where

import qualified Data.IntMap as M
import SymbolicMatch.Expr

type Env = M.IntMap Expr

init :: Env
init = M.empty

bind :: Int -> Expr -> Env -> Env
bind = M.insert

bindAll :: [(Int, Expr)] -> Env -> Env
bindAll ps env = foldl (\rest (n, e) -> M.insert n e rest) env ps

get :: Int -> Env -> Maybe Expr
get = M.lookup

unsafeGet :: Int -> Env -> String -> Expr
unsafeGet key env msg = 
    case M.lookup key env of
        Just e -> e
        Nothing -> error msg