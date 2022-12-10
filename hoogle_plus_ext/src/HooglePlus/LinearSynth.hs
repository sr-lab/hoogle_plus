module HooglePlus.LinearSynth where

import Types.Filtering
import HooglePlus.FilterTest
import Data.Maybe
import Data.List (sort, isInfixOf, nub, intercalate)
import Types.Environment
import HooglePlus.Example
import Types.Type
import Types.Common
import Text.Printf

import qualified SymbolicMatch.Expr as E
import SymbolicMatch.Match (matchExprsPretty, matchPairsPretty, MatchError (..))
import qualified SymbolicMatch.Samples as S
import qualified SymbolicMatch.Env as Env

import Debug.Trace

{-
  In FunctionSignature types, the type Maybe Int is written as
  (ArgTypeApp (Concrete "Maybe") (Concrete "Int"))
 -}
matchTypeToReturn :: FunctionSignature 
                  -> ArgumentType 
                  -> Maybe FunctionSignature
matchTypeToReturn sig ret' =
  matchTypes [] (_returnType sig) ret' >>= (\substs -> return $ applySubstsSig substs sig)
  
matchTypes :: [(String, ArgumentType)] 
            -> ArgumentType 
            -> ArgumentType 
            -> Maybe [(String, ArgumentType)]
matchTypes sts (Concrete n) (Concrete n')
  | n == n' = Just sts
  | otherwise = Nothing
matchTypes sts (Concrete _) (Polymorphic _) = Just sts
matchTypes sts (Polymorphic n) t = case lookup n sts of
  Just t' -> if t == t' then Just sts else trace ("This case") Nothing
  Nothing -> Just $ (n, t):sts
matchTypes sts (ArgTypeList t) (ArgTypeList t') = 
  matchTypes sts t t'
matchTypes sts (ArgTypeTuple ts) (ArgTypeTuple ts') =
  foldr 
    (\(t1, t2) r -> case r of 
      Just sts' -> matchTypes sts' t1 t2
      Nothing -> Nothing) 
    (Just sts)
    (zip ts ts')
matchTypes sts (ArgTypeApp t1 t2) (ArgTypeApp t1' t2') = do
  sts'  <- matchTypes sts t1 t1'
  sts'' <- matchTypes sts' t2 t2'
  return sts''
matchTypes _ _ _ = Nothing

applySubsts :: [(String, ArgumentType)] -> ArgumentType -> ArgumentType
applySubsts sts p@(Polymorphic n) = fromMaybe p (lookup n sts)
applySubsts _ c@(Concrete _) = c
applySubsts sts (ArgTypeList t) = ArgTypeList $ applySubsts sts t
applySubsts sts (ArgTypeTuple ts) = ArgTypeTuple $ map (applySubsts sts) ts
applySubsts sts (ArgTypeApp t1 t2) = 
  ArgTypeApp (applySubsts sts t1) (applySubsts sts t2)
applySubsts sts (ArgTypeFunc t1 t2) = 
  ArgTypeFunc (applySubsts sts t1) (applySubsts sts t2)

applySubstsSig :: [(String, ArgumentType)] -> FunctionSignature -> FunctionSignature
applySubstsSig sts sig = sig { _argsType = map (applySubsts sts) (_argsType sig)
                             , _returnType = applySubsts sts (_returnType sig)}

linearSynth :: [(String, FunctionSignature, Int)] -- env
            -> String           -- type string for the lambda being synthesized
            -> [(Id, RSchema)]  -- argList from original function
            -> (E.Expr -> Either MatchError [(Int, [E.Expr], E.Expr)]) -- a function
            -> Int              -- next sym to use
            -> [String]     -- lambdas
linearSynth env typeStr argList matchFn nextSym = 
  toString $ sortExprs $ filterValid $ filterSyms $ applyMatch $ toLam $ filterArgs exprs
    where
      -- Data needed by the synthesis pipeline
      goal :: FunctionSignature
      goal = parseTypeString typeStr
      
      tygArgs :: [(Id, ArgumentType, Int)]
      tygArgs = map 
        (\((n, t), i) -> (n, _returnType $ parseTypeString (show t), i)) 
        (zip argList (iterate (+1) 0))
        
      lamArgs :: [(Id, ArgumentType, Int)]
      lamArgs = zip3 
        ["x" ++ show i | i <- iterate (+1) 0] 
        (_argsType goal)
        (iterate (+1) (length tygArgs))

      argCandidates :: [(Id, ArgumentType, Int)]
      argCandidates = tygArgs ++ lamArgs

      exprs :: [E.Expr]
      exprs = concatMap
        (\(n, t, i) -> map (\es -> E.App (E.Var i) es) (giveArgs nextSym [] (_argsType t))) 
        env
        where
          -- picks arguments that match the given types
          giveArgs :: Int -> [E.Expr] -> [ArgumentType] -> [[E.Expr]]
          giveArgs _ incomplete [] = [incomplete]
          giveArgs symCount incomplete (ty:tys) = let
            cands = mapMaybe 
              (\(cName, cTy, cIndex) -> case matchTypes [] ty cTy of
                Nothing -> Nothing
                Just sts -> Just (cName, map (applySubsts sts) tys, cIndex))
              argCandidates
            candsAppended = map (\(exps, ct, ci) -> (incomplete ++ [E.Var ci], ct)) cands
              ++ [(incomplete ++ [E.Sym symCount], tys)]
            in
              concatMap (\(cexps, ct) -> giveArgs (symCount + 1) cexps ct) candsAppended

      -- Functions that implement the synthesis pipeline

      -- sort by relevance
      sortExprs :: [E.Expr] -> [E.Expr]
      sortExprs = id

      -- remove expressions that do not use all the lambda arguments
      filterArgs :: [E.Expr] -> [E.Expr]
      filterArgs es =  let
        vars = sort $ nub $ map (\(x,y,z)->z) lamArgs in
        filter (\e -> vars `isInfixOf` (sort $ nub $ E.variables e)) es

      -- remove expressions that have symbols to replace
      filterSyms :: [E.Expr] -> [E.Expr]
      filterSyms es = filter (null . E.symbols) es

      -- remove invalid expres (like Cons 1 1)
      filterValid :: [E.Expr] -> [E.Expr]
      filterValid es = filter S.validate es

      -- expr -> Lam args expr
      toLam :: [E.Expr] -> [E.Expr]
      toLam es = let argsInds = map (\(_,_,id) -> id) lamArgs in
        map (\e -> E.Lam argsInds e) es

      -- replace symbols if match
      applyMatch :: [E.Expr] -> [E.Expr]
      applyMatch es = mapMaybe 
        (\e -> case matchFn e of 
          Left _ -> Nothing
          Right cs -> Just $ E.replaceSyms cs e)
        es
        
      -- convert to string
      toString :: [E.Expr] -> [String]
      toString es = map (\e -> "(" ++ E.showExpr S.functionsNames e ++ ")") es