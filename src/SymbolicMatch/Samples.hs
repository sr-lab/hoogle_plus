module SymbolicMatch.Samples where

import SymbolicMatch.Expr
import qualified SymbolicMatch.Env as E

import Data.Maybe (listToMaybe)

z :: Expr
z = DataC "Z" []

s :: Expr -> Expr
s n = DataC "S" [n]

true :: Expr
true = DataC "Data.Bool.True" []

false :: Expr
false = DataC "Data.Bool.False" []

pair :: Expr -> Expr -> Expr
pair a b = DataC "Pair" [a, b]

cons :: Expr -> Expr -> Expr
cons h t = DataC "Cons" [h, t]

left :: Expr -> Expr
left l = DataC "Data.Either.Left" [l]

right :: Expr -> Expr
right r = DataC "Data.Either.Right" [r]

nothing :: Expr
nothing = DataC "Data.Maybe.Nothing" []

just :: Expr -> Expr
just e = DataC "Data.Maybe.Just" [e]

list1 :: Expr
list1 = DataC "Cons" [Lit (LitInt 1), listNil]

list2 :: Expr
list2 = DataC "Cons" [Lit (LitInt 2), list1]

list3 :: Expr
list3 = DataC "Cons" [Lit (LitInt 3), list2]


getConsIndex :: Expr -> Int -> Maybe Expr
getConsIndex (DataC "Nil" []) _ = Nothing
getConsIndex (DataC "Cons" [h, t]) i
  | i == 0 = Just h
  | otherwise = getConsIndex t (i-1) 
getConsIndex _ _  = Nothing

---------------------------
-- adapted from GHC.List --
---------------------------

-- init
initF :: Expr
initF =
  Lam
    [0] -- 0: l
    (Case (Var 0)
      [Alt "Nil"  [] (DataC "Nil" []),
       Alt "Cons" [1, 2] -- h: 1, t: 2
          (Case
            (Var 2)
            [Alt "Nil" [] (DataC "Nil" []), -- h': 3, t': 4 , init: -1
             Alt "Cons" [3, 4] (DataC "Cons" [Var 1, App (Var (-1)) [Var 2]])])])

-- head
headF :: Expr
headF =
  Lam
    [0] -- l: 0
    (Case (Var 0)
      [Alt "Nil" [] (DataC "Error" []),
       Alt "Cons" [1, 2] (Var 1)] -- h: 1, t: 2
    )

-- last
lastF :: Expr
lastF =
  Lam 
    [0] -- l: 0
    (Case (Var 0)
      [Alt "Nil" [] (DataC "Error" []),
       Alt "Cons" [1, 2] ( -- h: 1, t:2
         Case (Var 2)
          [Alt "Nil" [] (Var 1), -- h': 3, t': 4, last: -17
           Alt "Cons" [3, 4] (App (Var (-17)) [Var 2])]
       )]
    )

-- !!
indexF :: Expr
indexF =
  Lam 
    [0, 1] -- l: 0, n: 1
    (Case (Var 1)
      [Alt "Z" [] (App (Var (-2)) [Var 0]), -- n': 2, index: -18, tail: -19, head: -2
       Alt "S" [2] (App (Var (-18)) [App (Var (-19)) [Var 0], Var 2])]
    )

-- tail
tailF :: Expr 
tailF = 
  Lam
    [0]
    (Case (Var 0)
      [Alt "Nil" [] (DataC "Error" []),
       Alt "Cons" [1, 2] (Var 2)]
    )

-- length
lengthF :: Expr
lengthF =
  Lam
    [0]
    (Case (Var 0)
      [
        Alt "Nil" [] (DataC "Z" []), -- index: -4
        Alt "Cons" [1, 2] (DataC "S" [App (Var (-4)) [Var 2]])
      ]
    )

-- null
nullF :: Expr
nullF =
  Lam 
    [0]
    (Case (Var 0)
      [
        Alt "Nil" [] true,
        Alt "Cons" [1, 2] false
      ]
    )

-- append
appendF :: Expr
appendF =
  Lam
    [0, 1]  -- l1: 0, l2: 1 
      (Case (Var 0)
        [
          Alt "Nil" [] (Var 1), -- h: 2, t: 3, append: -5
          Alt "Cons" [2, 3] (DataC "Cons" [Var 2, App  (Var (-5)) [Var 3, Var 1]])
        ]
      )

-- reverse
reverseF :: Expr
reverseF =
  Lam
    [0] -- l: 0
    (Case (Var 0)
      [
        Alt "Nil" [] (DataC "Nil" []),
        Alt "Cons" [1, 2] -- h: 1, t: 2, append: -5, reverse: -6
          (App  (Var (-5))
          [
            App (Var (-6)) [Var 2],
            DataC "Cons" [Var 1, listNil]
          ]
        )
      ]
    )

reverseIterF :: Expr
reverseIterF =
  Lam
    [0, 1] -- l: 0, acc: 1
      (Case
        (Var 0)
        [
          Alt "Nil" [] (Var 1),
          Alt "Cons" [2, 3] -- h: 2, t: 3
            (App (Var (-7))
              [Var 3, DataC "Cons" [Var 2, Var 1]])
        ]
      )

-- take
takeF :: Expr
takeF =
  Lam
    [0, 1] -- n: 0, l: 1
    (Case
      (Var 0)
      [
        Alt "Z" [] (DataC "Nil" []),
        Alt "S" [2] -- n': 2
          (Case
            (Var 1)
            [
              Alt "Nil" [] (DataC "Nil" []), -- h: 3, t: 4, take: -10
              Alt "Cons" [3, 4] (DataC "Cons" [Var 3, App (Var (-10)) [Var 2, Var 4]])
            ]
          )
      ]
    )

-- zip
zipF :: Expr
zipF =
  Lam 
    [0, 1] -- l1: 0, l2: 1
    (Case 
      (Var 0)
      [
        Alt "Nil" [] listNil,
        Alt "Cons" [2, 3] -- h1: 2, t1: 3
          (Case 
            (Var 1)
            [
              Alt "Nil" [] listNil,
              Alt "Cons" [4, 5] -- h2: 4, t2: 5
                (DataC "Cons" 
                  [
                    DataC "Pair" [Var 2, Var 4], -- zip: -14
                    App (Var (-14)) [Var 3, Var 5]
                  ]
                )
            ]
          )
      ]
    )

-- replicate
replicateF :: Expr
replicateF =
  Lam 
    [0, 1] --n: 0, a: 1
    (Case 
      (Var 0)
      [
        Alt "Z" [] listNil, -- n': 2, replicate: 
        Alt "S" [2] $ cons (Var 1) (App (Var (-15)) [Var 2, Var 1])
      ]
    )

-- concat
concatF :: Expr
concatF =
  Lam [0]
    (Case (Var 0)
      [
        Alt "Nil" [] listNil,
        Alt "Cons" [1, 2] (App (Var $ -5) [Var 1, App (Var $ -40) [Var 2]])
      ]
    )

-- uncons
unconsF :: Expr
unconsF = Lam [0]
  (Case (Var 0) 
    [
      Alt "Nil" [] nothing,
      Alt "Cons" [1, 2] (just (pair (Var 1) (Var 2)))
    ]
  )

-- drop
dropF :: Expr
dropF = 
  Lam 
    [0, 1] -- n:0, l:1
    (Case (Var 0)
      [
        Alt "Z" [] (Var 1),
        Alt "S" [2] (
          Case (Var 1)
          [
            Alt "Nil" [] listNil,
            Alt "Cons" [3, 4] (App (Var $ -42) [Var 2, Var 4])
          ]
        )
      ]
    )

-- sum
sumF :: Expr
sumF = Lam [0]
  (Case (Var 0)
    [
      Alt "Nil" [] z,
      Alt "Cons" [1, 2] (App (Var $ -11) [Var 1, App (Var $ -45) [Var 2]])
    ]
  )

-- product
productF :: Expr
productF = Lam [0]
  (Case (Var 0)
    [
      Alt "Nil" [] (s z), -- product [] = 1
      Alt "Cons" [1, 2] (App (Var $ -44) [Var 1, App (Var $ -46) [Var 2]])
    ]
  )

-- maximum
maximumF :: Expr
maximumF = Lam [0]
  (Case (Var 0)
    [
      Alt "Nil" [] (DataC "Error" []),
      Alt "Cons" [1, 2] (App (Var $ -51) [Var 1, App (Var $ -47) [Var 2]])
    ]
  )

-- minimum
minimumF :: Expr
minimumF = Lam [0]
  (Case (Var 0)
    [
      Alt "Nil" [] (DataC "Error" []),
      Alt "Cons" [1, 2] (App (Var $ -52) [Var 1, App (Var $ -48) [Var 2]])
    ]
  )

-- and
andListF :: Expr
andListF = Lam [0]
  (Case (Var 0)
    [
      Alt "Nil" [] true,
      Alt "Cons" [1, 2] (App (Var $ -34) [Var 1, App (Var $ -54) [Var 2]])
    ]
  )


-- or
orListF :: Expr
orListF = Lam [0]
  (Case (Var 0)
    [
      Alt "Nil" [] true,
      Alt "Cons" [1, 2] (App (Var $ -35) [Var 1, App (Var $ -55) [Var 2]])
    ]
  )

allF :: Expr
allF = 
  Lam 
    [0, 1] -- 0:a->Bool, 1:[a]
    (Case (Var 1)
      [
        Alt "Nil" [] true,
        Alt "Cons" [2, 3] (
          Case (App (Var 0) [Var 2]) [
            Alt "Data.Bool.False" [] false,
            Alt "Data.Bool.True" [] (App (Var $ -76) [Var 0, Var 3])
          ]
        )
      ]
    )

anyF :: Expr
anyF = 
  Lam 
    [0, 1] -- 0:a->Bool, 1:[a]
    (Case (Var 1)
      [
        Alt "Nil" [] false,
        Alt "Cons" [2, 3] (
          Case (App (Var 0) [Var 2]) [
            Alt "Data.Bool.True" [] true,
            Alt "Data.Bool.False" [] (App (Var $ -77) [Var 0, Var 3])
          ]
        )
      ]
    )
  
zipWithF :: Expr
zipWithF = 
  Lam 
    [0, 1, 2] --0: a->b->c, 1:[a], 2:[b]
    (Case (Var 1)
      [
        Alt "Nil" [] listNil,
        Alt "Cons" [3, 4] (
          Case (Var 2) [
            Alt "Nil" [] listNil,
            Alt "Cons" [5, 6] (
              cons 
                (App (Var 0) [Var 3, Var 5]) 
                (App (Var $ -78) [Var 4, Var 6])
            )
          ]
        )
      ]
    )

-- elem
elemF :: Expr
elemF = Lam [0, 1]
  (Case (Var 1)
    [
      Alt "Nil" [] false,
      Alt "Cons" [2, 3] (
        Case (App (Var $ -79) [Var 0, Var 2]) 
        [
          Alt "Data.Bool.False" [] (App (Var $ -81) [Var 0, Var 3]),
          Alt "Data.Bool.True" [] true
        ])
    ])

-- notElem
notElemF :: Expr
notElemF = Lam [0, 1]
  (Case (App (Var $ -81) [Var 0, Var 1])
    [
      Alt "Data.Bool.True" [] false,
      Alt "Data.Bool.False" [] true
    ])

-- lookup
lookupF :: Expr
lookupF = Lam [0, 1] -- 0:a, 1:[(a, b)]
  (Case (Var 1) [
    Alt "Nil" [] nothing,
    Alt "Cons" [2, 3] 
      (Case (Var 2) [
        Alt "Pair" [4, 5] 
          (Case (App (Var $ -79) [Var 0, Var 4])[
            Alt "Data.Bool.True" [] (just $ Var 5),
            Alt "Data.Bool.False" [] (App (Var $ -83) [Var 0, Var 3])
          ])
      ])
  ])

-----------------------------
-- adapted from Data.Maybe --
-----------------------------

justF :: Expr
justF = Lam [0] (DataC "Data.Maybe.Just" [Var 0])

nothingF :: Expr
nothingF = Lam [] (DataC "Data.Maybe.Nothing" [])

-- fromMaybe
fromMaybeF :: Expr
fromMaybeF =
  Lam 
    [0, 1] -- a: 0, m: 1
    (Case 
      (Var 1)
      [
        Alt "Data.Maybe.Nothing" [] (Var 0), -- a': 2
        Alt "Data.Maybe.Just" [2] (Var 2)
      ]
    )

-- isJust 
isJustF :: Expr
isJustF =
  Lam 
    [0] -- m: 0
    (Case 
      (Var 0)
      [
        Alt "Data.Maybe.Nothing" [] false, -- v: 1
        Alt "Data.Maybe.Just" [1] true
      ] 
    )

-- isNothing
isNothingF :: Expr
isNothingF =
  Lam 
    [0] -- m: 0
    (Case 
      (Var 0)
      [
        Alt "Data.Maybe.Nothing" [] true, -- v: 1
        Alt "Data.Maybe.Just" [1] false
      ] 
    )

-- fromJust
fromJustF :: Expr
fromJustF = 
  Lam 
    [0] -- m: 0
    (Case 
      (Var 0)
      [
        Alt "Data.Maybe.Nothing" [] (DataC "Error" []), -- v: 1
        Alt "Data.Maybe.Just" [1] (Var 1)
      ] 
    )

-- listToMaybe
listToMaybeF :: Expr
listToMaybeF =
  Lam 
    [0] -- l: 0
    (Case 
      (Var 0)
      [
        Alt "Nil" [] (DataC "Data.Maybe.Nothing" []), -- h: 1, t: 2
        Alt "Cons" [1, 2] (DataC "Data.Maybe.Just" [Var 1])
      ]
    )

-- maybeToList
maybeToListF :: Expr
maybeToListF = 
  Lam 
    [0] -- m
    (Case 
      (Var 0)
      [
        Alt "Data.Maybe.Nothing" [] listNil,
        Alt "Data.Maybe.Just" [1] (cons (Var 0) listNil)
      ]
    )

-- catMaybes
catMaybesF :: Expr
catMaybesF =
  Lam 
    [0] -- l: 0
    (Case 
      (Var 0)
      [
        Alt "Nil" [] (DataC "Nil" []), -- h: 1, t: 2
        Alt "Cons" [1, 2] 
          (Case 
            (Var 1)
            [
              Alt "Data.Maybe.Nothing" [] (App (Var $ -26) [Var 2]),
              Alt "Data.Maybe.Just" [3] (DataC "Cons" [Var 3, App (Var $ -26) [Var 2]])
            ]
          )
      ]
    )
  
-- mapMaybes
mapMaybeF :: Expr
mapMaybeF = 
  Lam 
    [0, 1]
    (Case
      (Var 1)
      [
        Alt "Nil" [] listNil,
        Alt "Cons" [2, 3] (
          Case (App (Var 0) [Var 2]) 
          [ -- mapMaybes: -64
            Alt "Data.Maybe.Nothing" [] (App (Var $ -64) [Var 0, Var 3]),
            Alt "Data.Maybe.Just" [4] (DataC "Cons" [Var 4, App (Var $ -64) [Var 0, Var 3]])
          ]
        )
      ]
    )

-- maybe
maybeF :: Expr
maybeF = 
  Lam 
    [0, 1, 2] -- 0: default, 1:h.o., 2: maybe values
    (Case 
      (Var 2)
      [
        Alt "Data.Maybe.Nothing" [] (Var 0),
        Alt "Data.Maybe.Just" [3] (App (Var 1) [Var 3])
      ])

------------------------------
-- adapted from Data.Either --
------------------------------

-- data Either a b = Data.Either.Left a | Right b

-- Left
leftF :: Expr
leftF = Lam [0] (DataC "Data.Either.Left" [Var 0])

-- Right
rightF :: Expr
rightF = Lam [0] (DataC "Data.Either.Right" [Var 0])

-- either 
eitherF :: Expr
eitherF = 
  Lam [0, 1, 2]
    (Case (Var 2) 
      [
        Alt "Data.Either.Left"  [3] (App (Var 0) [Var 3]),
        Alt "Data.Either.Right" [3] (App (Var 1) [Var 3])
      ]
    )

-- lefts
leftsN :: Int
leftsN = -27
leftsF :: Expr
leftsF =
  Lam 
    [0]
    (Case 
      (Var 0)
      [
        Alt "Nil" [] listNil,
        Alt "Cons" [1, 2] (
          Case 
            (Var 1)
            [
              Alt "Data.Either.Left" [3] (cons (Var 3) (App (Var leftsN) [Var 2])),
              Alt "Data.Either.Right" [3] (App (Var leftsN) [Var 2])
            ]
        )
      ]
    )

-- rights
rightsN :: Int
rightsN = -28
rightsF :: Expr
rightsF =
  Lam 
    [0]
    (Case 
      (Var 0)
      [
        Alt "Nil" [] listNil,
        Alt "Cons" [1, 2] (
          Case 
            (Var 1)
            [
              Alt "Data.Either.Left" [3] (App (Var leftsN) [Var 2]),
              Alt "Data.Either.Right" [3] (cons (Var 3) (App (Var leftsN) [Var 2]))
            ]
        )
      ]
    )

-- Data.Either.fromLeft
fromLeftF :: Expr
fromLeftF = 
  Lam 
    [0, 1] -- default: 0, eith: 1 
    (Case 
      (Var 1)
      [
        Alt "Data.Either.Left" [2] (Var 2),
        Alt "Data.Either.Right" [2] (Var 0)
      ]
    )

-- fromRight
fromRightF :: Expr
fromRightF = 
  Lam 
    [0, 1] -- default: 0, eith: 1 
    (Case 
      (Var 1)
      [
        Alt "Data.Either.Left" [2] (Var 0),
        Alt "Data.Either.Right" [2] (Var 2)
      ]
    )

-- Data.Either.isLeft
isLeftF :: Expr
isLeftF =
  Lam 
    [0]
    (Case 
      (Var 0)
      [
        Alt "Data.Either.Left" [1] true,
        Alt "Data.Either.Right" [1] false
      ]
    )

-- isRight
isRightF :: Expr
isRightF =
  Lam 
    [0]
    (Case 
      (Var 0)
      [
        Alt "Data.Either.Left" [1] false,
        Alt "Data.Either.Right" [1] true
      ]
    )

-- partitionEithers buggy!
partitionEithersF :: Expr
partitionEithersF = 
  Lam 
    [0]
    (Case
      (Var 0)
      [
        Alt "Nil" [] (pair listNil listNil), -- h: 1, t: 2
        Alt "Cons" [1, 2] 
          (Case
            (Var 1)
            [
              Alt "Data.Either.Left" [3] 
                (Case 
                  (App (Var $ -33) [Var 2])
                  [
                    Alt "Pair" [4, 5] $ pair (cons (Var 3) (Var 4)) (Var 5)
                  ]
                ),
              Alt "Data.Either.Right" [3] 
                (Case 
                  (App (Var $ -33) [Var 2])
                  [
                    Alt "Pair" [4, 5] $ pair (Var 4) (cons (Var 3) (Var 5))
                  ]
                )
            ]
          )
      ]
    )

----------------------------
-- adapted from Data.Bool --
----------------------------
-- data Bool = True | False

-- True
trueF :: Expr
trueF = Lam [] true

-- False
falseF :: Expr
falseF = Lam [] false

-- (&&)
andF :: Expr 
andF =
  Lam [0, 1]
    (Case (Var 0)
      [
        Alt "Data.Bool.True" [] (Var 1),
        Alt "Data.Bool.False" [] false
      ]
    )

-- (||)
orF :: Expr 
orF =
  Lam [0, 1]
    (Case (Var 0)
      [
        Alt "Data.Bool.True" [] true,
        Alt "Data.Bool.False" [] (Var 1)
      ]
    )

-- not
notF :: Expr 
notF =
  Lam [0]
    (Case (Var 0)
      [
        Alt "Data.Bool.True" [] false,
        Alt "Data.Bool.False" [] true
      ]
    )

-- bool
boolF :: Expr 
boolF =
  Lam [0, 1, 2]
    (Case (Var 2)
      [
        Alt "Data.Bool.True" [] (Var 1),
        Alt "Data.Bool.False" [] (Var 0)
      ]
    )

-----------------------------
-- adapted from Data.Tuple --
-----------------------------
-- data Pair a b = Pair a b

-- fst
fstF :: Expr
fstF = Lam [0] (Case (Var 0) [Alt "Pair" [1, 2] (Var 1)])

-- snd
sndF :: Expr
sndF = Lam [0] (Case (Var 0) [Alt "Pair" [1, 2] (Var 2)])

-- swap
swapF :: Expr
swapF = Lam [0] (Case (Var 0) [Alt "Pair" [1, 2] (pair (Var 2) (Var 1))])

-- curry
curryF :: Expr -- 0:fun, 1:a, 2:b
curryF = Lam [0, 1, 2] (App (Var 0) [pair (Var 1) (Var 2)])

-- uncurry
uncurryF :: Expr
uncurryF = Lam [0, 1] (Case (Var 1) [Alt "Pair" [2, 3] (App (Var 0) [Var 2, Var 3])])

--------------------------------
-- adapted from Data.Function --
--------------------------------
-- id
idF :: Expr
idF = Lam [0] (Var 0)

-- const
constF :: Expr
constF = Lam [0, 1] (Var 0)

-- (.)
dotF :: Expr
dotF = Lam [0, 1, 2] -- 0:(b->c), 1:(a->b), 2:a
  (App (Var 0) [App (Var 1) [Var 2]])

-- flip
flipF :: Expr
flipF = Lam [0, 1, 2] --0:(a->b->c), 1:b, 2:a
  (App (Var 0) [Var 2, Var 1])

-- ($)
dollarF :: Expr
dollarF = Lam [0, 1] --0:(a->b), 1:a
  (App (Var 0) [Var 1])

-- (&)
andComerF :: Expr
andComerF = Lam [0, 1] --0:a, 1:a->b
  (App (Var 1)[Var 0])

--------------------------
-- adapted from GHC.Num --
--------------------------
-- instead of being treated as a typeclass, 
-- the methods from the Num class are implemented
-- for natural numbers, the unique numbers supported

-- (+)
addF :: Expr
addF =
  Lam
    [0, 1] -- n1: 0, n2: 1
    (Case
      (Var 0)
      [
        Alt "Z" [] (Var 1), -- n1': 2
        Alt "S" [2] (DataC "S" [App (Var (-11)) [Var 2, Var 1]])
      ]
    )

-- (-)
subF :: Expr
subF = 
  Lam 
    [0, 1] -- n1:0, n2:1 (n1-n2)
    (Case (Var 1)
      [
        Alt "Z" [] (Var 0),
        Alt "S" [2] (Case (Var 0)
          [
            Alt "Z" [] z,
            Alt "S" [3] (App (Var $ -43) [Var 3, Var 2])
          ]
        )
      ]
    )

-- (*)
mulF :: Expr
mulF =
  Lam 
    [0, 1]
    (Case (Var 0)
      [
        Alt "Z" [] z,
        Alt "S" [2] (App (Var $ -11) [Var 1, App (Var $ -44) [Var 2, Var 1]])
      ]
    )

--------------------------
-- adapted from Data.Eq --
--------------------------

-- (==)
eqF :: Expr
eqF = Poly table
  where
    table :: PolyTable
    table = [ PolyAlt [DataConsIn 0 ["Nil", "Cons"]] eqListF
            , PolyAlt [DataConsIn 1 ["Nil", "Cons"]] eqListF
            , PolyAlt [DataConsIn 0 [ "Data.Bool.True"
                                    , "Data.Bool.False"]] eqBoolF
            , PolyAlt [DataConsIn 1 [ "Data.Bool.True"
                                    , "Data.Bool.False"]] eqBoolF
            , PolyAlt [DataConsIn 0 ["Z", "S"]] eqNatF
            , PolyAlt [DataConsIn 1 ["Z", "S"]] eqNatF
            , PolyAlt [DataConsIn 0 ["Pair"]] eqPairF
            , PolyAlt [DataConsIn 1 ["Pair"]] eqPairF
            ]
    
    eqListF :: Expr
    eqListF = Lam [0, 1]
      (Case (Var 0)
        [ Alt "Nil" [] 
            (Case (Var 1) 
              [ Alt "Nil" [] true
              , Alt "Cons" [2, 3] false
              ]
            )
        , Alt "Cons" [2, 3] 
            (Case (Var 1) 
              [ Alt "Nil" [] false
              , Alt "Cons" [4, 5] (
                Case (App (Var $ -79) [Var 2, Var 4])
                [ Alt "Data.Bool.True" [] (App eqListF [Var 3, Var 5])
                , Alt "Data.Bool.False" [] false
                ])
              ])
        ])

    eqBoolF :: Expr
    eqBoolF = Lam [0, 1] (Case (Var 0) 
      [ Alt "Data.Bool.True" [] (
          Case (Var 1)
          [ Alt "Data.Bool.True" [] true
          , Alt "Data.Bool.False" [] false
          ])
      , Alt "Data.Bool.False" [] (
          Case (Var 1)
          [ Alt "Data.Bool.True" [] false
          , Alt "Data.Bool.False" [] true
          ])
      ])

    eqNatF :: Expr
    eqNatF = Lam [0, 1] (Case (Var 0)
      [ Alt "Z" [] (
          Case (Var 1) 
          [ Alt "Z" []  true
          , Alt "S" [2] false
          ])
      , Alt "S" [2] (
          Case (Var 1) 
          [ Alt "Z" [] false
          , Alt "S" [3] (App eqNatF [Var 2, Var 3])])
      ])

    eqPairF :: Expr
    eqPairF = Lam [0, 1] (Case (Var 0)
      [ Alt "Pair" [2, 3] (Case (Var 1) 
        [
          Alt "Pair" [4, 5] (App (Var $ -34) [ -- (&&)
            App (Var $ -79) [Var 2, Var 4], -- v2 == v4
            App (Var $ -79) [Var 3, Var 5]  -- v3 == v5
          ])
        ])])

-- (/=)
neqF :: Expr
neqF = Lam [0, 1] (Case (App (Var $ -79) [Var 0, Var 1])
  [ Alt "Data.Bool.True" [] false
  , Alt "Data.Bool.False" [] true
  ])

---------------------------
-- adapted from Data.Ord --
---------------------------

-- (<=)
lteF :: Expr
lteF = Lam [0, 1] -- v0 <= v1?
  (Case (Var 0)
    [
      Alt "Z" [] true,
      Alt "S" [2] (Case (Var 1)
        [
          Alt "Z" [] false,
          Alt "S" [3] (App (Var $ -49) [Var 2, Var 3])
        ]
      )
    ]
  )

-- (>=)
gteF :: Expr
gteF = Lam [0, 1] (App (Var $ -49) [Var 1, Var 0])

-- (<)
ltF :: Expr
ltF = Lam [0, 1] -- v0 <= v1?
  (Case (Var 0)
    [
      Alt "Z" [] (Case (Var 1)
        [
          Alt "Z" [] false,
          Alt "S" [2] true
        ]
      ),
      Alt "S" [2] (Case (Var 1)
        [
          Alt "Z" [] false,
          Alt "S" [3] (App (Var $ -50) [Var 2, Var 3])
        ]
      )
    ]
  )

-- (>)
gtF :: Expr
gtF = Lam [0, 1] (App (Var $ -50) [Var 1, Var 0])

-- max
maxF :: Expr
maxF = Lam [0, 1]
  (Case (App (Var $ -50) [Var 0, Var 1])
    [
      Alt "Data.Bool.True" [] (Var 1),
      Alt "Data.Bool.False" [] (Var 0)
    ]
  )
  
-- min
minF :: Expr
minF = Lam [0, 1]
  (Case (App (Var $ -50) [Var 0, Var 1])
    [
      Alt "Data.Bool.True" [] (Var 0),
      Alt "Data.Bool.False" [] (Var 1)
    ]
  )


makePairF :: Expr
makePairF = Lam [0, 1] (DataC "Pair" [Var 0, Var 1])

mapF :: Expr
mapF =
  Lam
    [0, 1] -- fn: 0, l: 1
    (Case
      (Var 1)
      [
        Alt "Nil" [] (DataC "Nil" []), -- h: 2, t: 3
        Alt "Cons" [2, 3] (DataC "Cons" [App (Var 0) [Var 2], App (Var (-8)) [Var 0, Var 3]])
      ])

filterF :: Expr
filterF =
  Lam
    [0, 1] -- fn: 0, l: 1
    (Case
      (Var 1)
      [
        Alt "Nil" [] (DataC "Nil" []),
        Alt "Cons" [2, 3] -- h: 2, t: 3
          (Case
            (App (Var 0) [Var 2])
            [
              Alt "Data.Bool.True" [] (DataC "Cons" [Var 2, App (Var (-9)) [Var 0, Var 3]]),
              Alt "Data.Bool.False" [] (App (Var (-9)) [Var 0, Var 3])
            ])
      ])

foldrF :: Expr
foldrF =
  Lam
    [0, 1, 2] -- f: 0, d: 1, l:2
    (Case
      (Var 2)
      [
        Alt "Nil" [] (Var 1), -- h: 3, t: 4
        Alt "Cons" [3, 4] (App (Var 0) [Var 3, App (Var (-12)) [Var 0, Var 1, Var 4]])
      ]
    )

oddF :: Expr
oddF =
  Lam 
    [0] -- n: 0
    (Case 
      (Var 0)
      [
        Alt "Z" [] false, -- n': 1, n'': 2
        Alt "S" [1] (Case (Var 1) [Alt "Z" [] true, Alt "S" [2] (App (Var (-13)) [Var 2])])
      ]
   )
   
nilF :: Expr
nilF = Lam [] (DataC "Nil" [])

consF :: Expr
consF = Lam [0, 1] (DataC "Cons" [Var 0, Var 1])

functionsInfo :: [(Int, Expr, String)]
functionsInfo =
  [ -- GHC.List
    (-1, initF, "GHC.List.init"),
    (-2, headF, "GHC.List.head"),
    (-4, lengthF, "GHC.List.length"),
    (-5, appendF, "(GHC.List.++)"),
    (-6, reverseF, "GHC.List.reverse"),
    (-8, mapF, "GHC.List.map"),
    (-9, filterF, "GHC.List.filter"),
    (-10, takeF, "GHC.List.take"),
    (-12, foldrF, "GHC.List.foldr"),
    (-14, zipF, "GHC.List.zip"),
    (-15, replicateF, "GHC.List.replicate"),
    (-17, lastF, "GHC.List.last"),
    (-18, indexF, "(GHC.List.!!)"),
    (-19, tailF, "GHC.List.tail"),
    (-20, nullF, "GHC.List.null"),
    (-45, sumF, "GHC.List.sum"),
    (-46, productF, "GHC.List.product"),
    (-47, maximumF, "GHC.List.maximum"),
    (-48, minimumF, "GHC.List.minimum"),
    (-54, andListF, "GHC.List.and"),
    (-55, orListF, "GHC.List.or"),
    (-40, concatF, "GHC.List.concat"),
    (-41, unconsF, "GHC.List.uncons"),
    (-42, dropF, "GHC.List.drop"),
    (-60, nilF, "Nil"), -- TYGAR uses Nil, instead of GHC.List.Nil
    (-61, consF, "Cons"),
    (-76, allF, "GHC.List.all"),
    (-77, anyF, "GHC.List.any"),
    (-78, zipWithF, "GHC.List.zipWith"),
    (-81, elemF, "GHC.List.elem"),
    (-82, notElemF, "GHC.List.notElem"),
    (-83, lookupF, "GHC.List.lookup"),

    -- Data.Maybe
    (-56, nothingF, "Data.Maybe.Nothing"),
    (-57, justF, "Data.Maybe.Just"),
    (-16, fromMaybeF, "Data.Maybe.fromMaybe"),
    (-21, isJustF, "Data.Maybe.isJust"),
    (-22, isNothingF, "Data.Maybe.isNothing"),
    (-23, listToMaybeF, "Data.Maybe.listToMaybe"),
    (-24, fromJustF, "Data.Maybe.fromJust"),
    (-25, maybeToListF, "Data.Maybe.maybeToList"),
    (-26, catMaybesF, "Data.Maybe.catMaybes"),
    (-64, mapMaybeF, "Data.Maybe.mapMaybe"),
    (-65, maybeF, "Data.Maybe.maybe"),
    
    -- Data.Either
    (-58, leftF, "Data.Either.Left"),
    (-59, rightF, "Data.Either.Right"),
    (leftsN, leftsF, "Data.Either.lefts"),
    (rightsN, rightsF, "Data.Either.rights"),
    (-29, fromLeftF, "Data.Either.fromLeft"),
    (-30, fromRightF, "Data.Either.fromRight"),
    (-31, isLeftF, "Data.Either.isLeft"),
    (-32, isRightF, "Data.Either.isRight"),
    (-33, partitionEithersF, "Data.Either.partitionEithers"),
    (-66, eitherF, "Data.Either.either"),

    -- Data.Bool
    (-62, trueF, "Data.Bool.True"),
    (-63, falseF, "Data.Bool.False"),
    (-34, andF, "(Data.Bool.&&)"),
    (-35, orF, "(Data.Bool.||)"),
    (-36, boolF, "Data.Bool.bool"),
    (-53, notF, "Data.Bool.not"),
    -- otherwise is not needed

    -- Data.Tuple
    (-37, fstF, "Data.Tuple.fst"),
    (-38, sndF, "Data.Tuple.snd"),
    (-39, swapF, "Data.Tuple.swap"),
    (-67, curryF, "Data.Tuple.curry"),
    (-68, uncurryF, "Data.Tuple.uncurry"),

    -- Data.Function
    (-3, idF, "Data.Function.id"),
    (-69, constF, "Data.Function.const"),
    (-70, dotF, "(Data.Function..)"),
    (-71, flipF, "Data.Function.flip"),
    (-72, dollarF, "(Data.Function.$)"),
    (-73, andComerF, "(Data.Function.&)"),
    -- fix, on need recursive let/definitions

    -- GHC.Num
    (-43, subF, "(GHC.Num.-)"),
    (-44, mulF, "(GHC.Num.*)"),
    (-11, addF, "(GHC.Num.+)"),
    -- TODO ...

    -- Data.Ord
    (-49, lteF, "(Data.Ord.<=)"),
    (-74, gteF, "(Data.Ord.>=)"),
    (-50, ltF, "(Data.Ord.<)"),
    (-75, gtF, "(Data.Ord.>)"),
    (-51, maxF, "Data.Ord.max"),
    (-52, minF, "Data.Ord.min"),
    -- compare is not interesting, returns ordering

    -- Data.Eq
    (-79, eqF, "(Data.Eq.==)"),
    (-80, neqF, "(Data.Eq./=)"),
    
    (-7, reverseIterF, "reverse-iter"),
    (-13, oddF, "odd")
  ]

-- pairs (id, expr) to use during the algorthm
functionsEnv :: E.Env
functionsEnv = E.bindAll (map (\(k, v, _)->(k, v)) functionsInfo) E.init

-- pairs (id, name) to use during printing
functionsNames :: [(Int, String)]
functionsNames = map (\(k, _, v)->(k, v)) functionsInfo

-- searches a function
lookupFun :: String -> Maybe Int
lookupFun id = listToMaybe $ map (\(int, _, _) -> int) $ filter (\(int, _, str)->str == id) functionsInfo

-- names and arity
dataConstrs :: [(String, Int)]
dataConstrs = [ ("Data.Either.Left", 1)
              , ("Data.Either.Right", 1)
              , ("Cons", 2)
              , ("Nil", 0)
              , ("Pair", 2)
              , ("S", 1)
              , ("Z", 0)
              , ("Data.Maybe.Just", 1)
              , ("Data.Maybe.Nothing", 0)
              , ("Data.Bool.True", 0)
              , ("Data.Bool.False", 0)]

dataConstrsNames :: [String]
dataConstrsNames = map fst dataConstrs

-- returns whether name is a data constructor or not
isDataC :: String -> Bool
isDataC name = name `elem` dataConstrsNames

-- a weak form of typechecking...
validate :: Expr -> Bool
validate (Lam es e) = validate e
validate (Poly _) = True -- Think about it
validate (Var _) = True
validate (Sym _) = True
validate (DataC n es) = 
  case lookup n dataConstrs of
    Nothing -> False
    Just arity -> length es == arity && all validate es &&
      (case n of 
        "Cons"-> case es !! 1 of 
          DataC n' es' -> n' == "Nil" || n' == "Cons"
          WildCard -> True 
          _ -> False
        "S" -> case head es of
          DataC n' es' -> n' == "S" || n' == "Z"
          WildCard -> True
          _ -> False
        _ -> True)
validate (App e es) = all validate (e:es)
validate (Case e alts) = validate e && all (\(Alt _ _ es) -> validate es) alts
validate WildCard = True
validate (Lit _) = True