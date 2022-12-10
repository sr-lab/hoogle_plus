{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module SymbolicMatch.Match
    ( matchExprs, matchExprsPretty, matchPairsPretty, MatchError(..))
     where
import Debug.Trace ( trace )
import SymbolicMatch.Expr ( Expr(..), Alt(..), final, symbols)
import qualified SymbolicMatch.Constr as C
import qualified SymbolicMatch.State as S
import qualified SymbolicMatch.Env as E
import SymbolicMatch.Eval (eval, evalMany)
import SymbolicMatch.Constr (pretty, prettyAll)

data MatchError = DepthReached
                | Mismatch
                | Exception String
                deriving (Show, Eq)

match :: Expr
    -> S.State
    -> Expr
    -> (S.State -> Either MatchError C.ConstrSet)
    -> Either MatchError C.ConstrSet

match _ state WildCard ct
  | S.reachMaxDepth state = Left DepthReached
  | otherwise = ct (S.incDepth state)

match WildCard state _ ct = Left $ Exception "Src should not be WildCard"

match (Lam n e) state dst ct = Left $ Exception "for now, do not match lambdas!"

match (Poly n) state dst ct = Left $ Exception "for now, do not match poly!"

match (Var n) state dst ct
  | S.reachMaxDepth state = Left DepthReached
  | otherwise = case S.get n state of
      Nothing -> Left $ Exception $ "Match variable " ++ show n ++ " not found"
      Just e -> match e (S.incDepth state) dst ct

match (Sym n) state dst ct
  | S.reachMaxDepth state = Left DepthReached  
  | otherwise = case S.lookupSym n state of
      Just e -> match e state dst ct
      Nothing -> ct (S.incDepth $ S.assign n dst state)

match src@(DataC n es) state dst ct
  | S.reachMaxDepth state = Left DepthReached
  | otherwise = case dst of
      DataC n' es'
        | n == n' && length es == length es' -> matchPairs (zip es es') (S.incDepth state) ct
        | otherwise -> Left Mismatch
      Sym n -> case S.lookupSym n state of
        Nothing -> ct (S.incDepth $ S.assign n src state)
        Just e -> match src state e ct
      _ -> Left Mismatch

match (Case e alts) state dst ct
  | S.reachMaxDepth state = Left DepthReached
  | otherwise = matchScrutinee e (Alt "Error" [] (DataC "Error" []):alts) state dst False ct
  where
    -- match the case scrutinee to the patterns
    -- and then match the alternative to the dst
    matchScrutinee :: Expr -> [Alt] -> S.State -> Expr -> Bool -> (S.State -> Either MatchError C.ConstrSet) -> Either MatchError C.ConstrSet
    matchScrutinee _ [] _ _ anyDepthReached _ = Left $ if anyDepthReached then DepthReached else Mismatch
    matchScrutinee scr ((Alt s ss e):alts) state dst anyDepthReached ct =
      let (syms, state') = S.genSyms (length ss) state
          ct' = \st -> match e (S.bindAll (zip ss syms) st) dst (\st' -> ct (S.newEnv st' (S.env st))) in
        case match scr state' (DataC s syms) ct' of
          Right cs' -> Right cs'
          Left DepthReached -> matchScrutinee scr alts (S.incDepth state) dst True ct
          Left _ -> matchScrutinee scr alts (S.incDepth state) dst anyDepthReached ct

match (App e es) state dst ct
  | S.reachMaxDepth state = Left DepthReached
  | otherwise =
      case e of
        (Lam ps e')
          | length es == length ps -> case evalMany state es of
                Left ex -> Left $ Exception ex
                Right es' -> let (pairs, args, state') = appArgs es' state
                                 state'' = S.bindAll (zip ps args) state'
                                 ct' = \st -> matchPairs pairs (S.newEnv st (S.env state')) ct in
                   match e' (S.incDepth state'') dst ct'
          | otherwise -> Left $ Exception $ "argument and param mismatch: " ++ show e ++ ";;;" ++ show es
        (Var n) -> let e' = S.unsafeGet n state ("App variable " ++ show n ++ " not found") in
            match (App e' es) (S.incDepth state) dst ct
        (Sym n) -> case evalMany state es of -- expects that the arguments of symbols applications does not have branches
            Left s -> Left (Exception s)
            Right es' -> case S.lookupApp n es' state of
              Just e -> match dst state e ct
              Nothing -> ct (S.incDepth (S.appAssign n es' dst state))
        (Poly table) -> matchPoly table es dst state ct False
        a -> Left $ Exception $ "App expression not a var nor lam" ++ show a
  where
    matchPoly :: [Expr]
      -> [Expr] 
      -> Expr
      -> S.State
      -> (S.State -> Either MatchError C.ConstrSet)
      -> Bool
      -> Either MatchError C.ConstrSet
    matchPoly [] _ _ _ _ depthReached 
      | depthReached = Left DepthReached
      | otherwise = Left $ Exception "no feasible abstraction"
    matchPoly (lam:lams) args dst state ct depthReached = 
      case match (App lam args) state dst ct of
        Left DepthReached -> matchPoly lams args dst state ct True
        Left Mismatch -> matchPoly lams args dst state ct depthReached
        Left (Exception s) -> Left (Exception s)
        Right cs -> Right cs

    appArgs :: [Expr] -> S.State -> ([(Expr, Expr)], [Expr], S.State)
    appArgs [] st = ([], [], st)
    appArgs (h:t) st = let (pairs, args, st') = appArgs t st in
      if final h 
        then (pairs, h:args, st') 
        else let (sym, st'') = S.genSym st' in
          ((h,sym):pairs, sym:args, st'')

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

matchPairsPretty :: Int 
                 -> [(Expr, Expr)] 
                 -> E.Env 
                 -> Either MatchError [(Int, [Expr], Expr)]
matchPairsPretty depth src env =
  matchPairs src (S.init env depth) idCont >>= \cs -> Right $ prettyAll cs