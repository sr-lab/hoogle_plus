LOG_DIR=logs
TIMEOUT1=60s
TIMEOUT2=90s
CNT1=10
CNT2=35
FLAG44=44
FLAG15=15

if (test $# -ne 0 && test $# -ne 1) || (test $# -eq 1 && test $1 -ne $FLAG44 && test $1 -ne $FLAG15) 
then 
    echo "Usage: bash $0 [$FLAG44|$FLAG15]"
    exit 1
fi

echo "Setup..."
rm -r $LOG_DIR 
mkdir $LOG_DIR 
stack build 1> /dev/null 2> /dev/null
stack exec -- hplus generate --preset partialfunctions 1> /dev/null 2> /dev/null

echo "Benchmarking..."

if test $# -eq 0 || (test $# -eq 1 && test $1 -eq $FLAG44)
then
    echo "Starting first set of 44 benchmarks" 
    timeout -k 1s $TIMEOUT1 stack exec -- hplus "[Either a b] -> Either a b" --example="[]" --cnt=$CNT1 --out=$LOG_DIR/firstRight.log 1> /dev/null 2> /dev/null
    echo 1/44
    timeout -k 1s $TIMEOUT1 stack exec -- hplus "[(a, b)] -> a" --example="[]" --cnt=$CNT1 --out=$LOG_DIR/firstKey.log 1> /dev/null 2> /dev/null
    echo 2/44
    timeout -k 1s $TIMEOUT1 stack exec -- hplus "[[[a]]] -> [a]" --example="[]" --cnt=$CNT1 --out=$LOG_DIR/flatten.log 1> /dev/null 2> /dev/null
    echo 3/44
    timeout -k 1s $TIMEOUT1 stack exec -- hplus "(a -> b) -> Int -> [a -> b]" --example="[]" --cnt=$CNT1 --out=$LOG_DIR/repl-funcs.log 1> /dev/null 2> /dev/null
    echo 4/44
    timeout -k 1s $TIMEOUT1 stack exec -- hplus "Int -> (Int, Int) -> Bool" --example="[]" --cnt=$CNT1 --out=$LOG_DIR/containsEdge.log 1> /dev/null 2> /dev/null
    echo 5/44
    timeout -k 1s $TIMEOUT1 stack exec -- hplus "(a -> b -> c) -> (a -> b) -> a -> c" --example="[]" --cnt=$CNT1 --out=$LOG_DIR/multiApp.log 1> /dev/null 2> /dev/null
    echo 6/44
    timeout -k 1s $TIMEOUT1 stack exec -- hplus "Int -> [a] -> [a]" --example="[]" --cnt=$CNT1 --out=$LOG_DIR/appendN.log 1> /dev/null 2> /dev/null
    echo 7/44
    timeout -k 1s $TIMEOUT1 stack exec -- hplus "[a -> a] -> (a -> a)" --example="[]" --cnt=$CNT1 --out=$LOG_DIR/pipe.log 1> /dev/null 2> /dev/null
    echo 8/44
    timeout -k 1s $TIMEOUT1 stack exec -- hplus "Int64 -> ByteString" --example="[]" --cnt=$CNT1 --out=$LOG_DIR/intToBS.log 1> /dev/null 2> /dev/null
    echo 9/44
    timeout -k 1s $TIMEOUT1 stack exec -- hplus "[a] -> [b] -> [[(a, b)]]" --example="[]" --cnt=$CNT1 --out=$LOG_DIR/cartProduct.log 1> /dev/null 2> /dev/null
    echo 10/44
    timeout -k 1s $TIMEOUT1 stack exec -- hplus "(a -> a) -> a -> Int -> a" --example="[]" --cnt=$CNT1 --out=$LOG_DIR/applyNtimes.log 1> /dev/null 2> /dev/null
    echo 11/44
    timeout -k 1s $TIMEOUT1 stack exec -- hplus "[a] -> (a -> Bool) -> a" --example="[]" --cnt=$CNT1 --out=$LOG_DIR/firstMatch.log 1> /dev/null 2> /dev/null
    echo 12/44
    timeout -k 1s $TIMEOUT1 stack exec -- hplus "Eq a => a -> [a] -> Maybe a" --example="[]" --cnt=$CNT1 --out=$LOG_DIR/mbElem.log 1> /dev/null 2> /dev/null
    echo 13/44
    timeout -k 1s $TIMEOUT1 stack exec -- hplus "(a -> Either b c) -> [a] -> ([b], [c])" --example="[]" --cnt=$CNT1 --out=$LOG_DIR/mapEither.log 1> /dev/null 2> /dev/null
    echo 14/44
    timeout -k 1s $TIMEOUT1 stack exec -- hplus "(a -> b) -> [a] -> b" --example="[]" --cnt=$CNT1 --out=$LOG_DIR/hoogle01.log 1> /dev/null 2> /dev/null
    echo 15/44
    timeout -k 1s $TIMEOUT1 stack exec -- hplus "(a -> b) -> [a] -> [(a, b)]" --example="[]" --cnt=$CNT1 --out=$LOG_DIR/zipWithResult.log 1> /dev/null 2> /dev/null
    echo 16/44
    timeout -k 1s $TIMEOUT1 stack exec -- hplus "String -> Char -> String" --example="[]" --cnt=$CNT1 --out=$LOG_DIR/splitStr.log 1> /dev/null 2> /dev/null
    echo 17/44
    timeout -k 1s $TIMEOUT1 stack exec -- hplus "[(a, b)] -> a -> b" --example="[]" --cnt=$CNT1 --out=$LOG_DIR/lookup.log 1> /dev/null 2> /dev/null
    echo 18/44
    timeout -k 1s $TIMEOUT1 stack exec -- hplus "a -> [Maybe a] -> a" --example="[]" --cnt=$CNT1 --out=$LOG_DIR/fromFirstMaybes.log 1> /dev/null 2> /dev/null
    echo 19/44
    timeout -k 1s $TIMEOUT1 stack exec -- hplus "(a -> b) -> [a] -> [b]" --example="[]" --cnt=$CNT1 --out=$LOG_DIR/map.log 1> /dev/null 2> /dev/null
    echo 20/44
    timeout -k 1s $TIMEOUT1 stack exec -- hplus "Maybe a -> a -> Maybe a" --example="[]" --cnt=$CNT1 --out=$LOG_DIR/maybe.log 1> /dev/null 2> /dev/null
    echo 21/44
    timeout -k 1s $TIMEOUT1 stack exec -- hplus "[Either a b] -> Either a [b] " --example="[]" --cnt=$CNT1 --out=$LOG_DIR/rights.log 1> /dev/null 2> /dev/null
    echo 22/44
    timeout -k 1s $TIMEOUT1 stack exec -- hplus "b -> (a -> b) -> [a] -> b" --example="[]" --cnt=$CNT1 --out=$LOG_DIR/mbAppFirst.log 1> /dev/null 2> /dev/null
    echo 23/44
    timeout -k 1s $TIMEOUT1 stack exec -- hplus "Either a (Either a b) -> Either a b" --example="[]" --cnt=$CNT1 --out=$LOG_DIR/mergeEither.log 1> /dev/null 2> /dev/null
    echo 24/44
    timeout -k 1s $TIMEOUT1 stack exec -- hplus "Bool -> a -> Maybe a" --example="[]" --cnt=$CNT1 --out=$LOG_DIR/test.log 1> /dev/null 2> /dev/null
    echo 25/44
    timeout -k 1s $TIMEOUT1 stack exec -- hplus "(a -> b, a -> c) -> a -> (b, c)" --example="[]" --cnt=$CNT1 --out=$LOG_DIR/multiAppPair.log 1> /dev/null 2> /dev/null
    echo 26/44
    timeout -k 1s $TIMEOUT1 stack exec -- hplus "a -> [a] -> ([a], [a])" --example="[]" --cnt=$CNT1 --out=$LOG_DIR/splitAtFirst.log 1> /dev/null 2> /dev/null
    echo 27/44
    timeout -k 1s $TIMEOUT1 stack exec -- hplus "(a->b)->(b->c)->[a]->[c]" --example="[]" --cnt=$CNT1 --out=$LOG_DIR/2partApp.log 1> /dev/null 2> /dev/null
    echo 28/44
    timeout -k 1s $TIMEOUT1 stack exec -- hplus "Eq a => a -> a -> Maybe a" --example="[]" --cnt=$CNT1 --out=$LOG_DIR/areEq.log 1> /dev/null 2> /dev/null
    echo 29/44
    timeout -k 1s $TIMEOUT1 stack exec -- hplus "Either a b -> Either a b -> Either a b" --example="[]" --cnt=$CNT1 --out=$LOG_DIR/eitherTriple.log 1> /dev/null 2> /dev/null
    echo 30/44
    timeout -k 1s $TIMEOUT1 stack exec -- hplus "(a -> Maybe b) -> [a] -> Maybe b" --example="[]" --cnt=$CNT1 --out=$LOG_DIR/mapMaybes.log 1> /dev/null 2> /dev/null
    echo 31/44
    timeout -k 1s $TIMEOUT1 stack exec -- hplus "[a] -> (a, [a])" --example="[]" --cnt=$CNT1 --out=$LOG_DIR/head-rest.log 1> /dev/null 2> /dev/null
    echo 32/44
    timeout -k 1s $TIMEOUT1 stack exec -- hplus "(a -> b) -> (a -> c) -> a -> (b, c)" --example="[]" --cnt=$CNT1 --out=$LOG_DIR/appBoth.log 1> /dev/null 2> /dev/null
    echo 33/44
    timeout -k 1s $TIMEOUT1 stack exec -- hplus "(a -> b, a) -> b" --example="[]" --cnt=$CNT1 --out=$LOG_DIR/applyPair.log 1> /dev/null 2> /dev/null
    echo 34/44
    timeout -k 1s $TIMEOUT1 stack exec -- hplus "Either a b -> (a->b) -> b" --example="[]" --cnt=$CNT1 --out=$LOG_DIR/resolveEither.log 1> /dev/null 2> /dev/null
    echo 35/44
    timeout -k 1s $TIMEOUT1 stack exec -- hplus "[a] -> (a,a)" --example="[]" --cnt=$CNT1 --out=$LOG_DIR/head-tail.log 1> /dev/null 2> /dev/null
    echo 36/44
    timeout -k 1s $TIMEOUT1 stack exec -- hplus "([(a,Int)] -> [(a,Int)]) -> [a] -> [Int] -> [Int]" --example="[]" --cnt=$CNT1 --out=$LOG_DIR/indexesOf.log 1> /dev/null 2> /dev/null
    echo 37/44
    timeout -k 1s $TIMEOUT1 stack exec -- hplus "(a -> b -> c -> d) -> a -> c -> b -> d" --example="[]" --cnt=$CNT1 --out=$LOG_DIR/app3.log 1> /dev/null 2> /dev/null
    echo 38/44
    timeout -k 1s $TIMEOUT1 stack exec -- hplus "(a -> b) -> (a, a) -> (b, b)" --example="[]" --cnt=$CNT1 --out=$LOG_DIR/both.log 1> /dev/null 2> /dev/null
    echo 39/44
    timeout -k 1s $TIMEOUT1 stack exec -- hplus "nt -> Int -> [a] -> ([a], [a])" --example="[]" --cnt=$CNT1 --out=$LOG_DIR/takeNdropM.log 1> /dev/null 2> /dev/null
    echo 40/44
    timeout -k 1s $TIMEOUT1 stack exec -- hplus "[Maybe a] -> a" --example="[]" --cnt=$CNT1 --out=$LOG_DIR/firstMaybe.log 1> /dev/null 2> /dev/null
    echo 41/44
    timeout -k 1s $TIMEOUT1 stack exec -- hplus "Maybe a -> b -> Either a b" --example="[]" --cnt=$CNT1 --out=$LOG_DIR/mbToEither.log 1> /dev/null 2> /dev/null
    echo 42/44
    timeout -k 1s $TIMEOUT1 stack exec -- hplus "[a] -> (a -> Bool) -> Int" --example="[]" --cnt=$CNT1 --out=$LOG_DIR/pred-match.log 1> /dev/null 2> /dev/null
    echo 43/44
    timeout -k 1s $TIMEOUT1 stack exec -- hplus "Int -> [Int]" --example="[]" --cnt=$CNT1 --out=$LOG_DIR/singleList.log 1> /dev/null 2> /dev/null
    echo 44/44
    echo "First set completed"    
else
    echo "Skipping first set of 44 benchmarks"
fi

if test $# -eq 0 || (test $# -eq 1 && test $1 -eq $FLAG15)
then
    echo "Starting second set of 15 benchmarks"
    timeout -k 1s $TIMEOUT2 stack exec -- hplus "[Int] -> [Int]" --example="[([[1, 2, 3]], [2, 3, 4])]" --cnt=$CNT2 --out=$LOG_DIR/mapAdd.log 1> /dev/null 2> /dev/null
    echo 1/15
    timeout -k 1s $TIMEOUT2 stack exec -- hplus "[Int] -> [Int]" --example="[([[1, 2]], [1, 4])]" --cnt=$CNT2 --out=$LOG_DIR/mapSquare.log 1> /dev/null 2> /dev/null
    echo 2/15
    timeout -k 1s $TIMEOUT2 stack exec -- hplus "[Int] -> [Int]" --example="[([[1, 2, 3]], [1, 2, 3, 4])]" --cnt=$CNT2 --out=$LOG_DIR/appendConst.log 1> /dev/null 2> /dev/null
    echo 3/15
    timeout -k 1s $TIMEOUT2 stack exec -- hplus "[Int] -> [Int]" --example="[([[1, 2, 3]], [1, 3])]" --cnt=$CNT2 --out=$LOG_DIR/filterDiff.log 1> /dev/null 2> /dev/null
    echo 4/15
    timeout -k 1s $TIMEOUT2 stack exec -- hplus "[Int] -> [Int]" --example="[([[1, 1, 0, 1, 2]], [1, 1])]" --cnt=$CNT2 --out=$LOG_DIR/getFirstOnes.log 1> /dev/null 2> /dev/null
    echo 5/15
    timeout -k 1s $TIMEOUT2 stack exec -- hplus "[Int] -> [Int]" --example="[([[1, 1, 0, 0, 1, 2]], [0, 0, 1, 2])]" --cnt=$CNT2 --out=$LOG_DIR/removeFirstOnes.log 1> /dev/null 2> /dev/null
    echo 6/15
    timeout -k 1s $TIMEOUT2 stack exec -- hplus "[Int] -> [Int] -> [Int]" --example="[([[0, 2, 4], [2, 4, 6], [2, 4])]" --cnt=$CNT2 --out=$LOG_DIR/listIntersect.log 1> /dev/null 2> /dev/null
    echo 7/15
    timeout -k 1s $TIMEOUT2 stack exec -- hplus "[Int] -> Int" --example="[([[1, 2, 0, 3, 0, 1]], 3)]" --cnt=$CNT2 --out=$LOG_DIR/indexConst.log 1> /dev/null 2> /dev/null
    echo 8/15
    timeout -k 1s $TIMEOUT2 stack exec -- hplus "[Int] -> Bool" --example="[([[2, 3, 4]], Data.Bool.True), ([[2, 1, 4]], Data.Bool.False)]" --cnt=$CNT2 --out=$LOG_DIR/allGreaterThan.log 1> /dev/null 2> /dev/null
    echo 9/15
    timeout -k 1s $TIMEOUT2 stack exec -- hplus "[Int] -> [Int]" --example="[([[0, 0, 4, 4, 3]], [4, 3])]" --cnt=$CNT2 --out=$LOG_DIR/dropConst.log 1> /dev/null 2> /dev/null
    echo 10/15
    timeout -k 1s $TIMEOUT2 stack exec -- hplus "[Int] -> [Int]" --example="[([[2, 0, 1, 3]], [2, 3])]" --cnt=$CNT2 --out=$LOG_DIR/filterGreaterThan.log 1> /dev/null 2> /dev/null
    echo 11/15
    timeout -k 1s $TIMEOUT2 stack exec -- hplus "[(Int, Int)] -> [(Int, Int)]" --example="[([[(1, 2), (3, 3), (4, 5)]], [(3, 3))]" --cnt=$CNT2 --out=$LOG_DIR/filterPairs.log 1> /dev/null 2> /dev/null
    echo 12/15
    timeout -k 1s $TIMEOUT2 stack exec -- hplus "[Int] -> [Int]" --example="[([[1, 2, 1, 3, 4, 4]], [1, 1])]" --cnt=$CNT2 --out=$LOG_DIR/filterEq.log 1> /dev/null 2> /dev/null
    echo 13/15
    timeout -k 1s $TIMEOUT2 stack exec -- hplus "[Int]" --example="[([], [3, 3, 3, 3, 3])]" --cnt=$CNT2 --out=$LOG_DIR/replicateConst.log 1> /dev/null 2> /dev/null
    echo 14/15
    timeout -k 1s $TIMEOUT2 stack exec -- hplus "[Int] -> [Int] -> [Int]" --example="[([[1, 2, 3], [3, 4, 5]], [4, 6, 8])]" --cnt=$CNT2 --out=$LOG_DIR/addElemsTwoLists.log 1> /dev/null 2> /dev/null
    echo 15/15
    echo "Second set completed"
else
    echo "Skipping second set of 15 benchmarks"
fi
echo "Done"