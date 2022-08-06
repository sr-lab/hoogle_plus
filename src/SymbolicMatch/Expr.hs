module SymbolicMatch.Expr where

data Expr = Lam [Int] Expr -- no currying
          | Var Int
          | Sym Int
          | DataC String [Expr]
          | Lit Lit
          | App Expr [Expr] -- no currying
          | Case Expr [Alt]
          | WildCard -- matches everything, used to match function applicatois
          deriving (Eq, Ord, Show)

data Alt = Alt String [Int] Expr deriving (Eq, Show, Ord)

data Lit = LitInt Int
         | LitString String 
         deriving (Eq, Show, Ord)

intToNat :: Int -> Expr
intToNat n =
  if n == 0
    then DataC "Z" []
    else DataC "S" [intToNat $ n-1]

natToInt :: Expr -> Int
natToInt (DataC "Z" []) = 0
natToInt (DataC "S" [n']) = 1 + natToInt n'
natToInt WildCard = 0
natToInt _ = error "natToLit"

listNil :: Expr
listNil = DataC "Nil" []

listToCons :: [Expr] -> Expr
listToCons [] = listNil
listToCons (h:t) = DataC "Cons" [h, listToCons t]

consToList :: Expr -> [Expr]
consToList (DataC "Nil" []) = []
consToList (DataC "Cons" [e, tail]) = e : consToList tail
consToList WildCard = [] -- FIXME
consToList _ =  error $ "consToList"

-- cons list of inductive integers to haskell lits of ints
ciToHs :: Expr -> [Int]
ciToHs (DataC "Nil" []) = []
ciToHs (DataC "Cons" [num, tail]) =
  natToInt num : ciToHs tail
ciToHs e = error $ "ciToHs"++ show e

hsToCi :: [Int] -> Expr
hsToCi [] = DataC "Nil" []
hsToCi (h:t) = DataC "Cons" [intToNat h, hsToCi t]


-- | returns the list of the names of symbols in the AST
symbols :: Expr -> [Int]
symbols (Sym n) = [n]
symbols (DataC _ es) = concatMap symbols es
symbols (App e es) = symbols e ++ concatMap symbols es
symbols WildCard = []
symbols (Lit _) = []
symbols (Lam _ e) = symbols e
symbols (Case e alts) = symbols e ++ concatMap (\(Alt _ _ e) -> symbols e) alts
symbols (Var _) = []

-- | returns the list of the names of variables in the AST
variables :: Expr -> [Int]
variables (Sym _) = []
variables (DataC _ es) = concatMap variables es
variables (App e es) = variables e ++ concatMap variables es
variables WildCard = []
variables (Lit _) = []
variables (Lam _ e) = variables e
variables (Case e alts) = variables e ++ concatMap (\(Alt _ _ e) -> variables e) alts
variables (Var n) = [n]

-- | returns True iff the expression is final
final :: Expr -> Bool
final (Sym _) = True
final (DataC _ es) = all final es
final (Lit _) = True
final WildCard = True -- FIXME?
final (Lam _ _) = True
final (App e es) = case e of 
  Sym _ -> all final es
  _ -> False
final _ = False

-- | replace all the occurences of src by dst inside expr
replace :: Expr -> Expr -> Expr -> Expr
replace expr src dst 
  | expr == src = dst
  | otherwise = case expr of
      Lam ss ex -> Lam ss (replace ex src dst)
      Var s -> expr
      Sym s -> expr
      DataC s exs -> DataC s (map (\e -> replace e src dst) exs)
      Lit lit -> expr
      App ex exs -> App (replace ex src dst) (map (\e -> replace e src dst) exs)
      Case ex alts -> Case (replace ex src dst) (map (\(Alt s ss e)->Alt s ss (replace e src dst)) alts)
      WildCard -> expr

replaceAll :: Expr -> [(Expr, Expr)] -> Expr
replaceAll e rpl = foldr (\(s,d) r -> replace r s d) e rpl

needsPar :: Expr -> Bool
needsPar (Lit _) = False
needsPar (Var _) = False
needsPar (Sym _) = False
needsPar (Lam _ _) = True
needsPar WildCard = False
needsPar (DataC n es) = n `notElem` ["Z", "S", "Cons", "Nil", "Pair"] && not (null es)
needsPar (App _ es) = not (null es)
needsPar (Case _ _) = True

-- convert an expression to a String
showExpr :: [(Int, String)] -> Expr -> String
showExpr _ WildCard = "_"
showExpr _ (Lam _ _) = error "Solutions not expected to have lambdas."
showExpr env (Var n) = case lookup n env of
  Nothing -> error "Variable id does not exist."
  Just s -> s
showExpr _ (Sym s) = '$':show s
showExpr env d@(DataC n es) = case n of
  "Nil" -> "[]"
  "Cons"
    | length es == 2 -> "[" ++ showCons env (es !! 0) (es !! 1) ++ "]"
    | otherwise -> error "showExpr: Cons does not have 2 args"
  "Z" -> "0"
  "S" -> show $ natToInt d
  "Pair" -> "(" ++ showExpr env (es !! 0) ++ ", " ++ showExpr env (es !! 1) ++ ")"
  _ -> n ++ foldr (\c r-> ' ':(if needsPar c then "(" ++ showExpr env c ++ ")" else showExpr env c) ++ r) "" es
  where 
    showCons :: [(Int, String)] -> Expr -> Expr -> String
    showCons env hd tl = case tl of
      (DataC "Nil" []) -> showExpr env hd
      WildCard -> showExpr env hd {- ++ ", ..." -} -- despite being useful, it does not suit the compilation in GHC...
      (DataC "Cons" args)
        | length args == 2 -> showExpr env hd ++ ", " ++ showCons env (args !! 0) (args !! 1)
        | otherwise -> error "showCons"
      _ -> error "showCons: not nil, cons, wildcard: (" ++ show tl ++ ")"
showExpr env (App e es) = showExpr env e ++ foldr (\c r-> ' ':(if needsPar c then "(" ++ showExpr env c ++ ")" else showExpr env c) ++ r) "" es
showExpr _ (Lit l) = show l
showExpr _ (Case _ _) = error "Solutions not expected to have cases."

-- replace the symbols of an expression
replaceSyms :: [(Int, [Expr], Expr)] -> Expr -> Expr
replaceSyms [] e = e
replaceSyms ((s, [], ex):t) e = replaceSyms t $ replace e (Sym s) ex
replaceSyms (_:t) e = replaceSyms t e