module HooglePlus.Example (Example(..), parseExample, programToExpr) where

import SymbolicMatch.Match
import SymbolicMatch.Expr
import SymbolicMatch.Samples (functionsInfo, lookupFun, isDataC)
import Types.Program
import Data.Maybe (listToMaybe)
import Data.List (partition, isPrefixOf)

import Debug.Trace (trace)

data Example = Example {
  input :: [Expr],
  output :: Expr
}

-- | convert example from a String to SymbolicMatch AST
parseExample :: String -> Maybe Example
parseExample str = Just Example {input = [], output = WildCard}

-- | convert from Synquid AST to SymbolicMatch AST, replacing args by example inputs
programToExpr :: UProgram -- solution in Synquid AST
              -> Example  -- example
              -> [String] -- argument list
              -> (UProgram, Expr) -- the program with symbols names replaced (Symbol....)
                                  -- and the expression to SymbolicMatch
programToExpr prog example argsNames = let 
  prog' = fst $ assignSyms prog 0 in
    (prog', programToExpr' prog' example)
  where 
    -- changes the names of the symbols (Symbol.symbol...) to S0, S1, ...
    -- to allow having more than on symbol per type
    assignSyms :: UProgram -> Int -> (UProgram, Int)
    assignSyms prog nextSym = case content prog of
      (PSymbol id)
        | isTygarSymbol id -> let prog' = prog {content = PSymbol $ "Sym" ++ show nextSym} in
            (prog', nextSym + 1)
        | otherwise -> (prog, nextSym)
      PApp id args -> let 
        (id', nextSym') = if isTygarSymbol id 
                            then ("Sym" ++ show nextSym, nextSym + 1) 
                            else (id, nextSym)
        (args', nextSym'') = assignSymsArgs args nextSym'
        prog' = prog {content = PApp id' args'} in
          (prog', nextSym'')
      _ -> error "assignSyms: solutions expected to be composed by vars and apps."
      where
        assignSymsArgs :: [UProgram] -> Int -> ([UProgram], Int)
        assignSymsArgs [] nextSym = ([], nextSym)
        assignSymsArgs (h:t) nextSym = let
          (h', nextSym')  = assignSyms h nextSym
          (t', nextSym'') = assignSymsArgs t nextSym' in
            (h':t', nextSym'')

        -- symbols in Tygar are constants from the Symbol module
        isTygarSymbol :: String -> Bool 
        isTygarSymbol name = "Symbol.symbol" `isPrefixOf` name

    -- returns the index of the argument: "arg2" -> 1
    lookupArg :: String -> Maybe Int
    lookupArg id
      | id `elem` argsNames = Just $ (read (drop (length "arg") id) :: Int) - 1 -- args start in 1
      | otherwise = Nothing

    -- FIXME now args cannot be functions, because one cant write a function as example
    programToExpr' :: UProgram -> Example -> Expr
    programToExpr' prog example = case content prog of
      PSymbol id
        | "Sym" `isPrefixOf` id -> Sym $ ((read (drop (length "Sym") id)) :: Int)
        | otherwise -> case lookupArg id of
            Just arg -> if length (input example) > arg then (input example) !! arg else error $ "error accessing argument list  (index is " ++ show arg ++ ")"
            Nothing -> case lookupFun id of
              Just fun -> Var fun
              Nothing -> error $ "\'" ++ id ++ "\'" ++ " is not a symbol, nor a function or argument"
      PApp id args -> case maybe (lookupArg id) Just (lookupFun id) of -- search both functions and variables
        Just var -> let args' = map (\p -> programToExpr' p example) args in
          App (Var var) args'
        Nothing
          | isDataC id -> let args' = map (\p -> programToExpr' p example) args in
              DataC id args'
          | otherwise -> error $ "\'" ++ id ++ "\'" ++ " is not a function nor argument nor data constructor"
      _ -> error "Not supposed to be a solution."