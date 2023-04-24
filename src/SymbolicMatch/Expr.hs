module SymbolicMatch.Expr where
import Text.Printf (printf)
import Data.List (intercalate)

data Expr = Lam ![Int] !Expr -- no currying
          | Poly ![Expr] -- lambda abstractions
          | Var !Int
          | Sym !Int
          | DataC !String ![Expr]
          | App !Expr ![Expr] -- no currying
          | Case !Expr ![Alt]
          | WildCard -- matches everything, used to match function applicatois
          deriving (Eq, Ord, Show)

data Alt = Alt !String ![Int] !Expr deriving (Eq, Show, Ord)

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
symbols (Lam _ e) = symbols e
symbols (Case e alts) = symbols e ++ concatMap (\(Alt _ _ e) -> symbols e) alts
symbols (Var _) = []
symbols (Poly lams) = concatMap (\(Lam _ e) -> symbols e) lams

-- | returns True iff the expression is final
final :: Expr -> Bool
final (Sym _) = True
final (DataC _ es) = all final es
final WildCard = True -- FIXME?
final (Lam _ _) = True
final (Poly _ ) = True
final (App e es) = case e of 
  Sym _ -> all (\e -> final e && null (symbols e)) es
  _ -> False
final _ = False

-- | returns the list of the names of variables in the AST
variables :: Expr -> [Int]
variables (Sym _) = []
variables (DataC _ es) = concatMap variables es
variables (App e es) = variables e ++ concatMap variables es
variables WildCard = []
variables (Lam _ e) = variables e
variables (Poly lams) = concatMap variables lams
variables (Case e alts) = variables e ++ concatMap (\(Alt _ _ e) -> variables e) alts
variables (Var n) = [n]

-- | replace all the occurences of src by dst inside expr
replace :: Expr -> Expr -> Expr -> Expr
replace expr src dst 
  | expr == src = dst
  | otherwise = case expr of
      Lam ss ex -> Lam ss (replace ex src dst)
      Poly lams -> Poly (map (\l -> replace l src dst) lams)
      Var s -> expr
      Sym s -> expr
      DataC s exs -> DataC s (map (\e -> replace e src dst) exs)
      App ex exs -> App (replace ex src dst) (map (\e -> replace e src dst) exs)
      Case ex alts -> Case (replace ex src dst) (map (\(Alt s ss e)->Alt s ss (replace e src dst)) alts)
      WildCard -> expr

replaceAll :: Expr -> [(Expr, Expr)] -> Expr
replaceAll e rpl = foldr (\(s,d) r -> replace r s d) e rpl

needsPar :: Expr -> Bool
needsPar (Var _) = False
needsPar (Sym _) = False
needsPar (Lam _ _) = True
needsPar (Poly _) = True
needsPar WildCard = False
needsPar (DataC n es) = n `notElem` ["Z", "S", "Cons", "Nil", "Pair"] && not (null es)
needsPar (App _ es) = not (null es)
needsPar (Case _ _) = True

-- convert an expression to a String
showExpr :: [(Int, String)] -> Expr -> String
showExpr env e = fst $ showExpr' 0 e
  where -- we print each wild card as a _n, where n is a new number for each
    showExpr' :: Int -> Expr -> (String, Int)
    showExpr' nextWild WildCard = ('_' : show nextWild, nextWild + 1)
    showExpr' nextWild (Lam args body) = let
        args' = intercalate " " (map (\i -> "x" ++ show i) args)
        body' = showExpr' nextWild body in
          (printf "\\%s -> %s" args' (fst body'), snd body')
    showExpr' _ (Poly _) = error "Solutions not expected to have poly."
    showExpr' nextWild (Var n) = case lookup n env of
      Nothing -> ('x':show n, nextWild)
      Just s -> (s, nextWild)
    showExpr' nextWild (Sym s) = ('$':show s, nextWild)
    showExpr' nextWild d@(DataC n es) = case n of
      "Nil" -> ("[]", nextWild)
      "Cons"
        | length es == 2 -> 
            let (str, nWild) = showCons nextWild (es !! 0) (es !! 1) in 
              ("[" ++ str ++ "]", nWild)
        | otherwise -> error "showExpr: Cons does not have 2 args"
      "Z" -> ("0", nextWild)
      "S" -> (show $ natToInt d, nextWild)
      "Pair" -> let (p1, n1) = showExpr' nextWild (es !! 0)
                    (p2, n2) = showExpr' n1 (es !! 1) in
          ("(" ++ p1 ++ ", " ++  p2 ++ ")", n2)
      _ -> let (p, w) = showArgs nextWild es in (n ++ p, w)
    showExpr' nextWild (App e es) = let 
        (p1, n1) = showExpr' nextWild e
        (p2, n2) = showArgs n1 es in
        (p1 ++ p2, n2)
    showExpr' _ (Case _ _) = error "Solutions not expected to have cases."

    showCons :: Int -> Expr -> Expr -> (String, Int)
    showCons nextWild hd tl = case tl of
      (DataC "Nil" []) -> showExpr' nextWild hd
      WildCard -> showExpr' nextWild hd {- ++ ", ..." -} -- despite being useful, it does not suit the compilation in GHC...
      (DataC "Cons" args)
        | length args == 2 -> let
            (p1, n1) = showExpr' nextWild hd
            (p2, n2) = showCons n1 (args !! 0) (args !! 1) in
          (p1 ++ ", " ++ p2, n2)
        | otherwise -> error "showCons"
      _ -> error $ "showCons: not nil, cons, wildcard: (" ++ show tl ++ ")"

    showArgs :: Int -> [Expr] -> (String, Int)
    showArgs nextWild [] = ("", nextWild)
    showArgs nextWild (h:t) = let
      (p1, n1) = showExpr' nextWild h
      (p2, n2) = showArgs n1 t
      p1' = if needsPar h then "(" ++ p1 ++ ")" else p1 
      in (" " ++ p1' ++ p2, n2)

-- replace the symbols of an expression
replaceSyms :: [(Int, [Expr], Expr)] -> Expr -> Expr
replaceSyms [] e = e
replaceSyms ((s, [], ex):t) e = replaceSyms t $ replace e (Sym s) ex
replaceSyms (_:t) e = replaceSyms t e