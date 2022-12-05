LOG_DIR=bench-logs-extension
TIMEOUT1=60s
TIMEOUT2=90s
CNT=35

echo "Setup..."
rm -r $LOG_DIR 1> /dev/null 2> /dev/null
mkdir $LOG_DIR 1> /dev/null 2> /dev/null
stack build 1> /dev/null 2> /dev/null
stack exec -- hplus generate --preset partialfunctions 1> /dev/null 2> /dev/null

echo "Benchmarking..."

echo "Starting first set of 44 benchmarks"
timeout $TIMEOUT1 stack exec -- hplus "[Either a b] -> Either a b" --example="[]" --cnt=100 --out=firstRight.log 1> /dev/null 2> /dev/null
echo 1/44
timeout $TIMEOUT1 stack exec -- hplus "[(a, b)] -> a" --example="[]" --cnt=100 --out=firstKey.log 1> /dev/null 2> /dev/null
echo 2/44
timeout $TIMEOUT1 stack exec -- hplus "[[[a]]] -> [a]" --example="[]" --cnt=100 --out=flatten.log 1> /dev/null 2> /dev/null
echo 3/44
timeout $TIMEOUT1 stack exec -- hplus "(a -> b) -> Int -> [a -> b]" --example="[]" --cnt=100 --out=repl-funcs.log 1> /dev/null 2> /dev/null
echo 4/44
timeout $TIMEOUT1 stack exec -- hplus "Int -> (Int, Int) -> Bool" --example="[]" --cnt=100 --out=containsEdge.log 1> /dev/null 2> /dev/null
echo 5/44
timeout $TIMEOUT1 stack exec -- hplus "(a -> b -> c) -> (a -> b) -> a -> c" --example="[]" --cnt=100 --out=multiApp.log 1> /dev/null 2> /dev/null
echo 6/44
timeout $TIMEOUT1 stack exec -- hplus "Int -> [a] -> [a]" --example="[]" --cnt=100 --out=appendN.log 1> /dev/null 2> /dev/null
echo 7/44
timeout $TIMEOUT1 stack exec -- hplus "[a -> a] -> (a -> a)" --example="[]" --cnt=100 --out=pipe.log 1> /dev/null 2> /dev/null
echo 8/44
timeout $TIMEOUT1 stack exec -- hplus "Int64 -> ByteString" --example="[]" --cnt=100 --out=intToBS.log 1> /dev/null 2> /dev/null
echo 9/44
timeout $TIMEOUT1 stack exec -- hplus "[a] -> [b] -> [[(a, b)]]" --example="[]" --cnt=100 --out=cartProduct.log 1> /dev/null 2> /dev/null
echo 10/44
timeout $TIMEOUT1 stack exec -- hplus "(a -> a) -> a -> Int -> a" --example="[]" --cnt=100 --out=applyNtimes.log 1> /dev/null 2> /dev/null
echo 11/44
timeout $TIMEOUT1 stack exec -- hplus "[a] -> (a -> Bool) -> a" --example="[]" --cnt=100 --out=firstMatch.log 1> /dev/null 2> /dev/null
echo 12/44
timeout $TIMEOUT1 stack exec -- hplus "Eq a => a -> [a] -> Maybe a" --example="[]" --cnt=100 --out=mbElem.log 1> /dev/null 2> /dev/null
echo 13/44
timeout $TIMEOUT1 stack exec -- hplus "(a -> Either b c) -> [a] -> ([b], [c])" --example="[]" --cnt=100 --out=mapEither.log 1> /dev/null 2> /dev/null
echo 14/44
timeout $TIMEOUT1 stack exec -- hplus "(a -> b) -> [a] -> b" --example="[]" --cnt=100 --out=hoogle01.log 1> /dev/null 2> /dev/null
echo 15/44
timeout $TIMEOUT1 stack exec -- hplus "(a -> b) -> [a] -> [(a, b)]" --example="[]" --cnt=100 --out=zipWithResult.log 1> /dev/null 2> /dev/null
echo 16/44
timeout $TIMEOUT1 stack exec -- hplus "String -> Char -> String" --example="[]" --cnt=100 --out=splitStr.log 1> /dev/null 2> /dev/null
echo 17/44
timeout $TIMEOUT1 stack exec -- hplus "[(a, b)] -> a -> b" --example="[]" --cnt=100 --out=lookup.log 1> /dev/null 2> /dev/null
echo 18/44
timeout $TIMEOUT1 stack exec -- hplus "a -> [Maybe a] -> a" --example="[]" --cnt=100 --out=fromFirstMaybes.log 1> /dev/null 2> /dev/null
echo 19/44
timeout $TIMEOUT1 stack exec -- hplus "(a -> b) -> [a] -> [b]" --example="[]" --cnt=100 --out=map.log 1> /dev/null 2> /dev/null
echo 20/44
timeout $TIMEOUT1 stack exec -- hplus "Maybe a -> a -> Maybe a" --example="[]" --cnt=100 --out=maybe.log 1> /dev/null 2> /dev/null
echo 21/44
timeout $TIMEOUT1 stack exec -- hplus "[Either a b] -> Either a [b] " --example="[]" --cnt=100 --out=rights.log 1> /dev/null 2> /dev/null
echo 22/44
timeout $TIMEOUT1 stack exec -- hplus "b -> (a -> b) -> [a] -> b" --example="[]" --cnt=100 --out=mbAppFirst.log 1> /dev/null 2> /dev/null
echo 23/44
timeout $TIMEOUT1 stack exec -- hplus "Either a (Either a b) -> Either a b" --example="[]" --cnt=100 --out=mergeEither.log 1> /dev/null 2> /dev/null
echo 24/44
timeout $TIMEOUT1 stack exec -- hplus "Bool -> a -> Maybe a" --example="[]" --cnt=100 --out=test.log 1> /dev/null 2> /dev/null
echo 25/44
timeout $TIMEOUT1 stack exec -- hplus "(a -> b, a -> c) -> a -> (b, c)" --example="[]" --cnt=100 --out=multiAppPair.log 1> /dev/null 2> /dev/null
echo 26/44
timeout $TIMEOUT1 stack exec -- hplus "a -> [a] -> ([a], [a])" --example="[]" --cnt=100 --out=splitAtFirst.log 1> /dev/null 2> /dev/null
echo 27/44
timeout $TIMEOUT1 stack exec -- hplus "(a->b)->(b->c)->[a]->[c]" --example="[]" --cnt=100 --out=2partApp.log 1> /dev/null 2> /dev/null
echo 28/44
timeout $TIMEOUT1 stack exec -- hplus "Eq a => a -> a -> Maybe a" --example="[]" --cnt=100 --out=areEq.log 1> /dev/null 2> /dev/null
echo 29/44
timeout $TIMEOUT1 stack exec -- hplus "Either a b -> Either a b -> Either a b" --example="[]" --cnt=100 --out=eitherTriple.log 1> /dev/null 2> /dev/null
echo 30/44
timeout $TIMEOUT1 stack exec -- hplus "(a -> Maybe b) -> [a] -> Maybe b" --example="[]" --cnt=100 --out=mapMaybes.log 1> /dev/null 2> /dev/null
echo 31/44
timeout $TIMEOUT1 stack exec -- hplus "[a] -> (a, [a])" --example="[]" --cnt=100 --out=head-rest.log 1> /dev/null 2> /dev/null
echo 32/44
timeout $TIMEOUT1 stack exec -- hplus "(a -> b) -> (a -> c) -> a -> (b, c)" --example="[]" --cnt=100 --out=appBoth.log 1> /dev/null 2> /dev/null
echo 33/44
timeout $TIMEOUT1 stack exec -- hplus "(a -> b, a) -> b" --example="[]" --cnt=100 --out=applyPair.log 1> /dev/null 2> /dev/null
echo 34/44
timeout $TIMEOUT1 stack exec -- hplus "Either a b -> (a->b) -> b" --example="[]" --cnt=100 --out=resolveEither.log 1> /dev/null 2> /dev/null
echo 35/44
timeout $TIMEOUT1 stack exec -- hplus "[a] -> (a,a)" --example="[]" --cnt=100 --out=head-tail.log 1> /dev/null 2> /dev/null
echo 36/44
timeout $TIMEOUT1 stack exec -- hplus "([(a,Int)] -> [(a,Int)]) -> [a] -> [Int] -> [Int]" --example="[]" --cnt=100 --out=indexesOf.log 1> /dev/null 2> /dev/null
echo 37/44
timeout $TIMEOUT1 stack exec -- hplus "(a -> b -> c -> d) -> a -> c -> b -> d" --example="[]" --cnt=100 --out=app3.log 1> /dev/null 2> /dev/null
echo 38/44
timeout $TIMEOUT1 stack exec -- hplus "(a -> b) -> (a, a) -> (b, b)" --example="[]" --cnt=100 --out=both.log 1> /dev/null 2> /dev/null
echo 39/44
timeout $TIMEOUT1 stack exec -- hplus "nt -> Int -> [a] -> ([a], [a])" --example="[]" --cnt=100 --out=takeNdropM.log 1> /dev/null 2> /dev/null
echo 40/44
timeout $TIMEOUT1 stack exec -- hplus "[Maybe a] -> a" --example="[]" --cnt=100 --out=firstMaybe.log 1> /dev/null 2> /dev/null
echo 41/44
timeout $TIMEOUT1 stack exec -- hplus "Maybe a -> b -> Either a b" --example="[]" --cnt=100 --out=mbToEither.log 1> /dev/null 2> /dev/null
echo 42/44
timeout $TIMEOUT1 stack exec -- hplus "[a] -> (a -> Bool) -> Int" --example="[]" --cnt=100 --out=pred-match.log 1> /dev/null 2> /dev/null
echo 43/44
timeout $TIMEOUT1 stack exec -- hplus "Int -> [Int]" --example="[]" --cnt=100 --out=singleList.log 1> /dev/null 2> /dev/null
echo 44/44
echo "First set completed"

echo "Starting second set of 15 benchmarks"
timeout -k 1s $TIMEOUT2 stack exec -- hplus "[Int] -> [Int]" --example="[([[1, 2, 3]], [2, 3, 4])]" --cnt=$CNT --out=$LOG_DIR/mapAdd.log 1> /dev/null 2> /dev/null
echo 1/15
timeout -k 1s $TIMEOUT2 stack exec -- hplus "[Int] -> [Int]" --example="[([[1, 2]], [1, 4])]" --cnt=$CNT --out=$LOG_DIR/mapSquare.log 1> /dev/null 2> /dev/null
echo 2/15
timeout -k 1s $TIMEOUT2 stack exec -- hplus "[Int] -> [Int]" --example="[([[1, 2, 3]], [1, 2, 3, 4])]" --cnt=$CNT --out=$LOG_DIR/appendConst.log 1> /dev/null 2> /dev/null
echo 3/15
timeout -k 1s $TIMEOUT2 stack exec -- hplus "[Int] -> [Int]" --example="[([[1, 2, 3]], [1, 3])]" --cnt=$CNT --out=$LOG_DIR/filterDiff.log 1> /dev/null 2> /dev/null
echo 4/15
timeout -k 1s $TIMEOUT2 stack exec -- hplus "[Int] -> [Int]" --example="[([[1, 1, 0, 1, 2]], [1, 1])]" --cnt=$CNT --out=$LOG_DIR/getFirstOnes.log 1> /dev/null 2> /dev/null
echo 5/15
timeout -k 1s $TIMEOUT2 stack exec -- hplus "[Int] -> [Int]" --example="[([[1, 1, 0, 0, 1, 2]], [0, 0, 1, 2])]" --cnt=$CNT --out=$LOG_DIR/removeFirstOnes.log 1> /dev/null 2> /dev/null
echo 6/15
timeout -k 1s $TIMEOUT2 stack exec -- hplus "[Int] -> [Int] -> [Int]" --example="[([[0, 2, 4], [2, 4, 6], [2, 4])]" --cnt=$CNT --out=$LOG_DIR/listIntersect.log 1> /dev/null 2> /dev/null
echo 7/15
timeout -k 1s $TIMEOUT2 stack exec -- hplus "[Int] -> Int" --example="[([[1, 2, 0, 3, 0, 1]], 3)]" --cnt=$CNT --out=$LOG_DIR/indexConst.log 1> /dev/null 2> /dev/null
echo 8/15
timeout -k 1s $TIMEOUT2 stack exec -- hplus "[Int] -> Bool" --example="[([[2, 3, 4]], Data.Bool.True), ([[2, 1, 4]], Data.Bool.False)]" --cnt=$CNT --out=$LOG_DIR/allGreaterThan.log 1> /dev/null 2> /dev/null
echo 9/15
timeout -k 1s $TIMEOUT2 stack exec -- hplus "[Int] -> [Int]" --example="[([[0, 0, 4, 4, 3]], [4, 3])]" --cnt=$CNT --out=$LOG_DIR/dropConst.log 1> /dev/null 2> /dev/null
echo 10/15
timeout -k 1s $TIMEOUT2 stack exec -- hplus "[Int] -> [Int]" --example="[([[2, 0, 1, 3]], [2, 3])]" --cnt=$CNT --out=$LOG_DIR/filterGreaterThan.log 1> /dev/null 2> /dev/null
echo 11/15
timeout -k 1s $TIMEOUT2 stack exec -- hplus "[(Int, Int)] -> [(Int, Int)]" --example="[([[(1, 2), (3, 3), (4, 5)]], [(3, 3))]" --cnt=$CNT --out=$LOG_DIR/filterPairs.log 1> /dev/null 2> /dev/null
echo 12/15
timeout -k 1s $TIMEOUT2 stack exec -- hplus "[Int] -> [Int]" --example="[([[1, 2, 1, 3, 4, 4]], [1, 1])]" --cnt=$CNT --out=$LOG_DIR/filterEq.log 1> /dev/null 2> /dev/null
echo 13/15
timeout -k 1s $TIMEOUT2 stack exec -- hplus "[Int]" --example="[([], [3, 3, 3, 3, 3])]" --cnt=$CNT --out=$LOG_DIR/replicateConst.log 1> /dev/null 2> /dev/null
echo 14/15
timeout -k 1s $TIMEOUT2 stack exec -- hplus "[Int] -> [Int] -> [Int]" --example="[([[1, 2, 3], [3, 4, 5]], [4, 6, 8])]" --cnt=$CNT --out=$LOG_DIR/addElemsTwoLists.log 1> /dev/null 2> /dev/null
echo 15/15
echo "Second set completed"
echo "Done"