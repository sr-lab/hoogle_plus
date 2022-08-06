module HooglePlus.Example ( Example(..)
                          , parseExample
                          , programToExpr
                          , symbolsDecls
                          , isSymbolWithPrefix
                          , isSymbol
                          , lookupSymType
                          , removeModulePrefix) where

import SymbolicMatch.Match
import SymbolicMatch.Expr
import SymbolicMatch.Samples (functionsInfo, lookupFun, isDataC, pair)
import Types.Program
import Data.Maybe (listToMaybe)
import Data.List (partition, isPrefixOf)

import Text.Parsec hiding (runParser)
import Text.Parsec.String (Parser)
import Text.Parsec.Token
import Text.Parsec.Language (emptyDef)

import Text.Printf (printf)

import Debug.Trace (trace)

symbolsInfo :: [(String, String)]
symbolsInfo = [
  ("symbolGen", "a")
  --("symbolInt", "Int"),
  --("symbolListInt", "[Int]")--,
  --("symbolMaybe", "Maybe a"),
  --("symbolEither", "Either a b")
  ]

symbolModule :: String
symbolModule = "Symbol"

symbolPrefix = symbolModule ++ "."

symbolsModPrefix :: [String]
symbolsModPrefix = map (\(s,_) -> printf "%s.%s" symbolModule s) symbolsInfo

symbolsDecls :: [String]
symbolsDecls = (printf "module %s" symbolModule) : map (\(n,t) -> printf "%s :: %s" n t) symbolsInfo

isSymbolWithPrefix :: String -> Bool
isSymbolWithPrefix s = s `elem` symbolsModPrefix

lookupSymType :: String -> String
lookupSymType sym = case lookup sym symbolsInfo of
  Just res -> res
  Nothing -> error $ printf "Symbol %s not found." sym

isSymbol :: String -> Bool
isSymbol s = s `elem` map fst symbolsInfo

removeModulePrefix :: String -> String
removeModulePrefix symName
  | symbolPrefix `isPrefixOf` symName = drop (length symbolPrefix) symName
  | otherwise = error "Symbol name does not have prefix."

data Example = Example {
  input :: [Expr],
  output :: Expr
} deriving Show

-- | convert example from a String to SymbolicMatch AST
parseExample :: String -> Either String Example
parseExample str = case parse expr "" str of
  Right example -> trace (showExpr [] example) $ case example of 
    DataC "Pair" [e1, e2] -> case e1 of 
      list@(DataC n _) 
        | n `elem` ["Cons", "Nil"] -> Right Example { input = consToList list
                                             , output = e2}
        | otherwise -> Left "first element of an example must be a list" 
      _ -> Left "first element of an example must be a list" 
    _ -> Left "example must be a pair"
  Left err -> Left $ show err
  where
    lexer :: TokenParser ()
    lexer = makeTokenParser style
      where
        style = emptyDef { identStart = letter
                         , identLetter = alphaNum
                         , reservedNames = []}

    expr :: Parser Expr
    expr = try pair <|> dataConstr <|> nat <|> list <|> parens lexer expr <|> wildcard

    -- expression that does not need parens
    insideExpr :: Parser Expr
    insideExpr =  (do x <- identifier lexer; return $ DataC x [])
              <|> nat
              <|> pair
              <|> list

    nat :: Parser Expr
    nat = do str <- many1 digit; return $ intToNat (read str)

    wildcard :: Parser Expr
    wildcard = char '_' >> return WildCard

    list :: Parser Expr
    list = do
      l <- brackets lexer $ commaSep lexer expr
      return $ listToCons l

    pair :: Parser Expr
    pair = parens lexer $ do
      e1 <- expr
      spaces; char ','; spaces
      e2 <- expr
      return $ SymbolicMatch.Samples.pair e1 e2

    dataConstr :: Parser Expr
    dataConstr = do
      datac <- identifier lexer
      spaces
      exps <- many $ (parens lexer expr) <|> insideExpr
      return $ DataC datac exps 

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
    -- changes the names of the symbols (Symbol.symbol...) to Sym0, Sym1, ...
    -- to allow having more than one symbol per type
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