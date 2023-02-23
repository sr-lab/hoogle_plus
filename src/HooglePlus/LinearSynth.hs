module HooglePlus.LinearSynth where

import Types.Filtering
import HooglePlus.FilterTest
import Data.Maybe
import Data.List (sort, isInfixOf, nub, intercalate, sortOn)
import Types.Environment
import HooglePlus.Example
import Types.Type
import Types.Common
import Text.Printf

import qualified SymbolicMatch.Expr as E
import SymbolicMatch.Match (matchPairsPretty, MatchError (..))
import qualified SymbolicMatch.Samples as S
import qualified SymbolicMatch.Env as Env
import Control.Monad.State
import Debug.Trace
import System.Timeout(timeout)

data SearchState = SearchState {
    stAllParams :: [(Id, ArgumentType, Int)], -- params from both Hoogle+ and the new lambda
    stEnv :: [(String, FunctionSignature, Int)],
    stMaxLevel :: Int,
    stNextSym :: Int,
    stSolutions :: [(Int, E.Expr)] -- (level, expr)
}

--  In FunctionSignature types, the type Maybe Int is written as
--  (ArgTypeApp (Concrete "Maybe") (Concrete "Int"))
matchTypeToReturn :: FunctionSignature 
                  -> ArgumentType 
                  -> Maybe FunctionSignature
matchTypeToReturn sig ret' = do
  substs <- matchTypes [] (_returnType sig) ret' 
  if all (\s -> True {-checkSubst s (_constraints sig)-}) substs then
    return $ applySubstsSig substs sig
  else
    Nothing

checkSubst :: (String, ArgumentType) -> [TypeConstraint] -> Bool
checkSubst (var, ty) constrs =
  let var2 = "Polymorphic " ++ show var in 
    case filter (\(TypeConstraint var' _) -> var2 == var') constrs of
      [] -> True -- var is not constrained
      (TypeConstraint _ tyClass):t
        | null t -> case ty of
          (Concrete name) -> case lookup tyClass instances of
            Nothing -> trace ("LinearSynth.hs: No class found") True
            Just insts -> name `elem` insts
          _ -> True
        | otherwise -> error "Typeclass constrained more than once"
  where 
    instances :: [(String, [String])]
    instances = [ ("Num", ["Int", "Float", "Int32", "Int64"])
                , ("Ord", ["Int", "Float", "Int32", "Int64"])
                , ("Eq",  ["Int", "Float", "Int32", "Int64"])] -- Fixme complete
  
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

-- fixme remove?
isPolymorphic :: ArgumentType -> Bool
isPolymorphic (Concrete _) = False
isPolymorphic (Polymorphic _) = True
isPolymorphic (ArgTypeList t) = isPolymorphic t
isPolymorphic (ArgTypeTuple ts) = all isPolymorphic ts
isPolymorphic (ArgTypeApp t1 t2) = isPolymorphic t2
isPolymorphic (ArgTypeFunc t1 t2) = isPolymorphic t1 && isPolymorphic t2

-- returns true if it has a function argument
isHigherOrderSign :: FunctionSignature -> Bool
isHigherOrderSign FunctionSignature{_argsType = ats} = 
  any (\t -> case t of {ArgTypeFunc _ _ -> True; _ -> False}) ats

isList :: ArgumentType -> Bool
isList (ArgTypeList _) = True
isList _ = False

isTuple :: ArgumentType -> Bool
isTuple (ArgTypeTuple _) = True
isTuple _ = False

linearSynth :: [(String, FunctionSignature, Int)] -- env
            -> String           -- type string for the lambda being synthesized
            -> [(Id, RSchema)]  -- argList from original function
            -> [([E.Expr], E.Expr)] -- input output examples
            -> Int              -- next sym to use
            -> IO [String]     -- lambdas
linearSynth env typeStr argList ioExamples nextSym = do
  let expr = E.Sym nextSym
  let initSt = SearchState{ stAllParams = argCandidates
                          , stEnv = env
                          , stMaxLevel = 2
                          , stNextSym = nextSym
                          , stSolutions = []}
  let exprs = stSolutions $ execState (completeExpr expr [(nextSym, _returnType goal, 0)] False 0) initSt
  let toMatch = map snd $ sortOn fst $ filterArgs $ exprs
  matched <- applyMatch toMatch
  return $ toString $ map toLam $ take 10 $ filterValid $ filterSyms $ matched
    where
      -- Data needed by the synthesis pipeline
      tygArgs :: [(Id, ArgumentType, Int)]
      tygArgs = map 
        (\((n, t), i) -> (n, _returnType $ parseTypeString (show t), i)) 
        (zip argList (iterate (+1) 0))
      
      goal = parseTypeString typeStr

      lamIds :: [Int]
      lamIds =  take (length (_argsType goal)) $ iterate (+1) (length tygArgs)

      lamArgs :: [(Id, ArgumentType, Int)]
      lamArgs = zip3 
        ["x" ++ show i | i <- iterate (+1) 0] 
        (_argsType goal)
        lamIds

      argCandidates :: [(Id, ArgumentType, Int)]
      argCandidates = tygArgs ++ lamArgs

      -- Functions that implement the synthesis pipeline
      -- remove expressions that do not use all the lambda arguments
      filterArgs :: [(Int, E.Expr)] -> [(Int, E.Expr)]
      filterArgs es =  let
        vars = sort $ nub $ map (\(x,y,z)->z) lamArgs in
        filter (\(_,e) -> vars `isInfixOf` (sort $ nub $ E.variables e)) es

      -- remove expressions that have symbols to replace
      filterSyms :: [E.Expr] -> [E.Expr]
      filterSyms es = filter (null . E.symbols) es

      -- remove invalid expres (like Cons 1 1)
      filterValid :: [E.Expr] -> [E.Expr]
      filterValid es = filter S.validate es

      -- expr -> Lam args expr
      toLam :: E.Expr -> E.Expr
      toLam p = E.Lam lamIds p

      -- replace symbols if match
      applyMatch :: [E.Expr] -> IO [E.Expr]
      applyMatch [] = return []
      applyMatch (e:es) = do
        let pairs = 
              map
                (\(args, val) -> 
                  (foldr 
                    (\(varId, argVal) r -> E.replace r (E.Var varId) argVal) e (zip lamIds args), val))
                ioExamples
        res <- timeout 100000 $ (let r = matchPairsPretty 200 pairs S.functionsEnv in r `seq` return r)
        case res of
          Just (Right cs) -> do
            remaining <- applyMatch es
            return $ (E.replaceSyms cs e) : remaining
          _ -> applyMatch es
        
      -- convert to string
      toString :: [E.Expr] -> [String]
      toString = map (\e -> "(" ++ E.showExpr S.functionsNames e ++ ")")
        
      -- complete the symbols of an expressions
      completeExpr :: E.Expr 
                    -> [(Int, ArgumentType, Int)] -- list of symbols to replace (id, type, level)
                    -> Bool -- if expr contains a symbol that will be replaced only by match, not by complete
                            -- relevant because we want at most 1 symbol
                    -> Int -- level of the expressions (0 if variables, 1 is 1 app and 2 if has at least 2 applications)
                    -> State SearchState ()
      completeExpr expr [] symbolToMatch eLevel = do -- no more symbols to replace, so expr is complete
        state <- get
        put state{stSolutions = (eLevel, expr) : stSolutions state}
      completeExpr expr ((symbolId, symbolType, symbolLevel):symbols) symbolToMatch eLevel = do
        state <- get
        -- all the arguments that can be used in place of symbolId
        let candidatesArgs = mapMaybe 
                (\(aId, aType, aIndex) ->
                  case matchTypes [] aType symbolType of
                    Just sts
                      | null sts -> 
                        let expr' = E.replace expr (E.Sym symbolId) (E.Var aIndex) in
                          Just (completeExpr expr' symbols symbolToMatch eLevel)
                      -- not supposed to replace type variables in arg types
                      | otherwise -> Nothing 
                    Nothing -> Nothing) 
                (stAllParams state)
        
        -- all the components that can be applied in place of symbolId
        let candidatesApps = mapMaybe 
                (\(_, cType, cIndex) ->
                  case matchTypeToReturn cType symbolType of
                    Just cType' -> let
                      next = stNextSym state
                      newIds = [n | n <- [ next .. (length (_argsType cType') + next - 1)]]
                      newSyms = [E.Sym n | n <- newIds]
                      expr' = E.replace expr (E.Sym symbolId) (E.App (E.Var cIndex) newSyms)
                      symbols' = zip3 newIds (_argsType cType') (repeat $ symbolLevel + 1) in 
                        Just (completeExpr expr' (symbols ++ symbols') symbolToMatch (symbolLevel + 1))
                    Nothing -> Nothing) 
                (stEnv state)
        
        -- update nextSymbol
        put state{stNextSym = stNextSym state + 10} -- FIXME corrigir valor exato
        
        -- do all actions
        sequence_ candidatesArgs
        -- add app nodes only its symbols are not so deep
        when (symbolLevel < stMaxLevel state) $ sequence_ candidatesApps
        -- if there are no symbols left for match, we can add one here
        when (not symbolToMatch && not (isList symbolType) && not (isTuple symbolType)) $ completeExpr expr symbols True eLevel
        return ()