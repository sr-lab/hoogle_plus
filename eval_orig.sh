LOG_DIR=bench-logs-original
TIMEOUT=60s
CNT=10

echo "Setup..."
rm -r $LOG_DIR
mkdir $LOG_DIR
stack build 1> /dev/null 2> /dev/null
stack exec -- hplus generate --preset partialfunctions 1> /dev/null 2> /dev/null

echo "Benchmarking..."
timeout -k 1s $TIMEOUT stack exec -- hplus "[Either a b] -> Either a b" --cnt=$CNT --out=$LOG_DIR/firstRight.log 1> /dev/null 2> /dev/null
echo 1/44               
timeout -k 1s $TIMEOUT stack exec -- hplus "[(a, b)] -> a" --cnt=$CNT --out=$LOG_DIR/firstKey.log 1> /dev/null 2> /dev/null
echo 2/44               
timeout -k 1s $TIMEOUT stack exec -- hplus "[[[a]]] -> [a]" --cnt=$CNT --out=$LOG_DIR/flatten.log 1> /dev/null 2> /dev/null
echo 3/44               
timeout -k 1s $TIMEOUT stack exec -- hplus "(a -> b) -> Int -> [a -> b]" --cnt=$CNT --out=$LOG_DIR/repl-funcs.log 1> /dev/null 2> /dev/null
echo 4/44               
timeout -k 1s $TIMEOUT stack exec -- hplus "Int -> (Int, Int) -> Bool" --cnt=$CNT --out=$LOG_DIR/containsEdge.log 1> /dev/null 2> /dev/null
echo 5/44               
timeout -k 1s $TIMEOUT stack exec -- hplus "(a -> b -> c) -> a -> c" --cnt=$CNT --out=$LOG_DIR/multiApp.log 1> /dev/null 2> /dev/null
echo 6/44               
timeout -k 1s $TIMEOUT stack exec -- hplus "Int -> [a] -> [a]" --cnt=$CNT --out=$LOG_DIR/appendN.log 1> /dev/null 2> /dev/null
echo 7/44               
timeout -k 1s $TIMEOUT stack exec -- hplus "[a -> a] -> (a -> a)" --cnt=$CNT --out=$LOG_DIR/pipe.log 1> /dev/null 2> /dev/null
echo 8/44               
timeout -k 1s $TIMEOUT stack exec -- hplus "Int64 -> ByteString" --cnt=$CNT --out=$LOG_DIR/intToBS.log 1> /dev/null 2> /dev/null
echo 9/44               
timeout -k 1s $TIMEOUT stack exec -- hplus "[a] -> [b] -> [[(a, b)]]" --cnt=$CNT --out=$LOG_DIR/cartProduct.log 1> /dev/null 2> /dev/null
echo 10/44              
timeout -k 1s $TIMEOUT stack exec -- hplus "(a -> a) -> a -> Int -> a" --cnt=$CNT --out=$LOG_DIR/applyNtimes.log 1> /dev/null 2> /dev/null
echo 11/44              
timeout -k 1s $TIMEOUT stack exec -- hplus "[a] -> (a -> Bool) -> a" --cnt=$CNT --out=$LOG_DIR/firstMatch.log 1> /dev/null 2> /dev/null
echo 12/44              
timeout -k 1s $TIMEOUT stack exec -- hplus "Eq a => a -> [a] -> Maybe a" --cnt=$CNT --out=$LOG_DIR/mbElem.log 1> /dev/null 2> /dev/null
echo 13/44              
timeout -k 1s $TIMEOUT stack exec -- hplus "(a -> Either b c) -> [a] -> ([b], [c])" --cnt=$CNT --out=$LOG_DIR/mapEither.log 1> /dev/null 2> /dev/null
echo 14/44              
timeout -k 1s $TIMEOUT stack exec -- hplus "(a -> b) -> [a] -> b" --cnt=$CNT --out=$LOG_DIR/hoogle01.log 1> /dev/null 2> /dev/null
echo 15/44              
timeout -k 1s $TIMEOUT stack exec -- hplus "(a -> b) -> [a] -> [(a, b)]" --cnt=$CNT --out=$LOG_DIR/zipWithResult.log 1> /dev/null 2> /dev/null
echo 16/44              
timeout -k 1s $TIMEOUT stack exec -- hplus "String -> Char -> String" --cnt=$CNT --out=$LOG_DIR/splitStr.log 1> /dev/null 2> /dev/null
echo 17/44              
timeout -k 1s $TIMEOUT stack exec -- hplus "[(a, b)] -> a -> b" --cnt=$CNT --out=$LOG_DIR/lookup.log 1> /dev/null 2> /dev/null
echo 18/44              
timeout -k 1s $TIMEOUT stack exec -- hplus "a -> [Maybe a] -> a" --cnt=$CNT --out=$LOG_DIR/fromFirstMaybes.log 1> /dev/null 2> /dev/null
echo 19/44              
timeout -k 1s $TIMEOUT stack exec -- hplus "(a -> b) -> [a] -> [b]" --cnt=$CNT --out=$LOG_DIR/map.log 1> /dev/null 2> /dev/null
echo 20/44              
timeout -k 1s $TIMEOUT stack exec -- hplus "Maybe a -> a -> Maybe a" --cnt=$CNT --out=$LOG_DIR/maybe.log 1> /dev/null 2> /dev/null
echo 21/44              
timeout -k 1s $TIMEOUT stack exec -- hplus "[Either a b] -> Either a [b] " --cnt=$CNT --out=$LOG_DIR/rights.log 1> /dev/null 2> /dev/null
echo 22/44              
timeout -k 1s $TIMEOUT stack exec -- hplus "b -> (a -> b) -> [a] -> b" --cnt=$CNT --out=$LOG_DIR/mbAppFirst.log 1> /dev/null 2> /dev/null
echo 23/44              
timeout -k 1s $TIMEOUT stack exec -- hplus "Either a (Either a b) -> Either a b" --cnt=$CNT --out=$LOG_DIR/mergeEither.log 1> /dev/null 2> /dev/null
echo 24/44              
timeout -k 1s $TIMEOUT stack exec -- hplus "Bool -> a -> Maybe a" --cnt=$CNT --out=$LOG_DIR/test.log 1> /dev/null 2> /dev/null
echo 25/44              
timeout -k 1s $TIMEOUT stack exec -- hplus "(a -> b, a -> c) -> a -> (b, c)" --cnt=$CNT --out=$LOG_DIR/multiAppPair.log 1> /dev/null 2> /dev/null
echo 26/44              
timeout -k 1s $TIMEOUT stack exec -- hplus "a -> [a] -> ([a], [a])" --cnt=$CNT --out=$LOG_DIR/splitAtFirst.log 1> /dev/null 2> /dev/null
echo 27/44              
timeout -k 1s $TIMEOUT stack exec -- hplus "(a->b)->(b->c)->[a]->[c]" --cnt=$CNT --out=$LOG_DIR/2partApp.log 1> /dev/null 2> /dev/null
echo 28/44              
timeout -k 1s $TIMEOUT stack exec -- hplus "Eq a => a -> a -> Maybe a" --cnt=$CNT --out=$LOG_DIR/areEq.log 1> /dev/null 2> /dev/null
echo 29/44              
timeout -k 1s $TIMEOUT stack exec -- hplus "Either a b -> Either a b -> Either a b" --cnt=$CNT --out=$LOG_DIR/eitherTriple.log 1> /dev/null 2> /dev/null
echo 30/44              
timeout -k 1s $TIMEOUT stack exec -- hplus "(a -> Maybe b) -> [a] -> Maybe b" --cnt=$CNT --out=$LOG_DIR/mapMaybes.log 1> /dev/null 2> /dev/null
echo 31/44              
timeout -k 1s $TIMEOUT stack exec -- hplus "[a] -> (a, [a])" --cnt=$CNT --out=$LOG_DIR/head-rest.log 1> /dev/null 2> /dev/null
echo 32/44              
timeout -k 1s $TIMEOUT stack exec -- hplus "(a -> b) -> (a -> c) -> a -> (b, c)" --cnt=$CNT --out=$LOG_DIR/appBoth.log 1> /dev/null 2> /dev/null
echo 33/44              
timeout -k 1s $TIMEOUT stack exec -- hplus "(a -> b, a) -> b" --cnt=$CNT --out=$LOG_DIR/applyPair.log 1> /dev/null 2> /dev/null
echo 34/44              
timeout -k 1s $TIMEOUT stack exec -- hplus "Either a b -> (a->b) -> b" --cnt=$CNT --out=$LOG_DIR/resolveEither.log 1> /dev/null 2> /dev/null
echo 35/44              
timeout -k 1s $TIMEOUT stack exec -- hplus "[a] -> (a,a)" --cnt=$CNT --out=$LOG_DIR/head-tail.log 1> /dev/null 2> /dev/null
echo 36/44              
timeout -k 1s $TIMEOUT stack exec -- hplus "([(a,Int)] -> [(a,Int)]) -> [a] -> [Int] -> [Int]" --cnt=$CNT --out=$LOG_DIR/indexesOf.log 1> /dev/null 2> /dev/null
echo 37/44              
timeout -k 1s $TIMEOUT stack exec -- hplus "(a -> b -> c -> d) -> a -> c -> b -> d" --cnt=$CNT --out=$LOG_DIR/app3.log 1> /dev/null 2> /dev/null
echo 38/44              
timeout -k 1s $TIMEOUT stack exec -- hplus "(a -> b) -> (a, a) -> (b, b)" --cnt=$CNT --out=$LOG_DIR/both.log 1> /dev/null 2> /dev/null
echo 39/44              
timeout -k 1s $TIMEOUT stack exec -- hplus "nt -> Int -> [a] -> ([a], [a])" --cnt=$CNT --out=$LOG_DIR/takeNdropM.log 1> /dev/null 2> /dev/null
echo 40/44              
timeout -k 1s $TIMEOUT stack exec -- hplus "[Maybe a] -> a" --cnt=$CNT --out=$LOG_DIR/firstMaybe.log 1> /dev/null 2> /dev/null
echo 41/44              
timeout -k 1s $TIMEOUT stack exec -- hplus "Maybe a -> b -> Either a b" --cnt=$CNT --out=$LOG_DIR/mbToEither.log 1> /dev/null 2> /dev/null
echo 42/44              
timeout -k 1s $TIMEOUT stack exec -- hplus "[a] -> (a -> Bool) -> Int" --cnt=$CNT --out=$LOG_DIR/pred-match.log 1> /dev/null 2> /dev/null
echo 43/44              
timeout -k 1s $TIMEOUT stack exec -- hplus "Int -> [Int]" --cnt=$CNT --out=$LOG_DIR/singleList.log 1> /dev/null 2> /dev/null
echo 44/44              

## -y em todos; CNT???