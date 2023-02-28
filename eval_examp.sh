LOG_DIR=logs
TIMEOUT=90s
CNT=35

echo "Setup..."
rm -rf $LOG_DIR 
if (test $? -ne 0); then echo "Error removing directory"; exit 1; fi

mkdir $LOG_DIR 
if (test $? -ne 0); then echo "Error creating directory"; exit 1; fi

stack build 1> /dev/null 2> /dev/null
if (test $? -ne 0); then echo "Error building hoolge_plus"; exit 1; fi

stack exec -- hplus generate --preset partialfunctions 1> /dev/null 2> /dev/null
if (test $? -ne 0); then echo "Error generating database"; exit 1; fi

echo "Benchmarking..."

timeout -k 1s $TIMEOUT stack exec -- hplus --json='{"query":"[Int] -> [Int]", "inExamples":[{"inputs":["[1, 2, 3]"],"output":"[2, 3, 4]"}]}' --cnt=$CNT --out=$LOG_DIR/mapAdd.log  1> /dev/null 2> /dev/null
echo 1/26
timeout -k 1s $TIMEOUT stack exec -- hplus --json='{"query":"[Int] -> [Int]", "inExamples":[{"inputs":["[1, 2]"],"output":"[1, 4]"}]}' --cnt=$CNT --out=$LOG_DIR/mapSquare.log 1> /dev/null 2> /dev/null
echo 2/26
timeout -k 1s $TIMEOUT stack exec -- hplus --json='{"query":"[Int] -> [Int]", "inExamples":[{"inputs":["[1, 2, 3]"],"output":"[1, 2, 3, 1000]"}]}' --cnt=$CNT --out=$LOG_DIR/appendConst.log 1> /dev/null 2> /dev/null
echo 3/26
timeout -k 1s $TIMEOUT stack exec -- hplus --json='{"query":"[Int] -> [Int]", "inExamples":[{"inputs":["[1, 2, 3]"],"output":"[1, 3]"}]}' --cnt=$CNT --out=$LOG_DIR/filterDiff.log 1> /dev/null 2> /dev/null
echo 4/26
timeout -k 1s $TIMEOUT stack exec -- hplus --json='{"query":"[Int] -> [Int]", "inExamples":[{"inputs":["[1, 1, 0, 1, 2]"],"output":"[1, 1]"}]}' --cnt=$CNT --out=$LOG_DIR/getFirstOnes.log 1> /dev/null 2> /dev/null
echo 5/26
timeout -k 1s $TIMEOUT stack exec -- hplus --json='{"query":"[Int] -> [Int]", "inExamples":[{"inputs":["[1, 1, 0, 0, 1, 2]"],"output":"[0, 0, 1, 2]"}]}' --cnt=$CNT --out=$LOG_DIR/removeFirstOnes.log 1> /dev/null 2> /dev/null
echo 6/26
timeout -k 1s $TIMEOUT stack exec -- hplus --json='{"query":"[Int] -> [Int] -> [Int]", "inExamples":[{"inputs":["[0, 2, 4]","[2, 4, 6"],"output":"[2, 4]"}]}' --cnt=$CNT --out=$LOG_DIR/listIntersect.log 1> /dev/null 2> /dev/null
echo 7/26
timeout -k 1s $TIMEOUT stack exec -- hplus --json='{"query":"[a] -> a", "inExamples":[{"inputs":["[1, 2, 0, 3, 0, 1]"],"output":"3"}]}' --cnt=$CNT --out=$LOG_DIR/indexConst.log 1> /dev/null 2> /dev/null
echo 8/26
timeout -k 1s $TIMEOUT stack exec -- hplus --json='{"query":"[Int] -> Bool", "inExamples":[{"inputs":["[2, 3, 4]"],"output":"Data.Bool.True"},{"inputs":["[2, 1, 4]"],"output":"Data.Bool.False"}]}' --cnt=$CNT --out=$LOG_DIR/allGreaterThan.log 1> /dev/null 2> /dev/null
echo 9/26
timeout -k 1s $TIMEOUT stack exec -- hplus --json='{"query":"[Int] -> [Int]", "inExamples":[{"inputs":["[0, 0, 4, 4, 3]"],"output":"[4, 3]"}]}' --cnt=$CNT --out=$LOG_DIR/dropConst.log 1> /dev/null 2> /dev/null
echo 10/26
timeout -k 1s $TIMEOUT stack exec -- hplus --json='{"query":"[Int] -> [Int]", "inExamples":[{"inputs":["[2, 0, 1, 3]"],"output":"[2, 3]"}]}' --cnt=$CNT --out=$LOG_DIR/filterGreaterThan.log 1> /dev/null 2> /dev/null
echo 11/26
timeout -k 1s $TIMEOUT stack exec -- hplus --json='{"query":"[(Int, Int)] -> [(Int, Int)]", "inExamples":[{"inputs":["[(1, 2), (2, 2), (3, 0)]"],"output":"[(2, 2)"}]}' --cnt=$CNT --out=$LOG_DIR/filterPairs.log 1> /dev/null 2> /dev/null
echo 12/26
timeout -k 1s $TIMEOUT stack exec -- hplus --json='{"query":"[Int] -> [Int]", "inExamples":[{"inputs":["[1, 2, 1, 3, 4, 4]"],"output":"[1, 1]"}]}' --cnt=$CNT --out=$LOG_DIR/filterEq.log 1> /dev/null 2> /dev/null
echo 13/26
timeout -k 1s $TIMEOUT stack exec -- hplus --json='{"query":"Int -> [Int]", "inExamples":[{"inputs":[1],"output":"[1, 1]"}]}' --cnt=$CNT --out=$LOG_DIR/replicateConst.log 1> /dev/null 2> /dev/null
echo 14/26
timeout -k 1s $TIMEOUT stack exec -- hplus --json='{"query":"[Int] -> [Int] -> [Int]", "inExamples":[{"inputs":["[1, 2, 3]","[3, 4, 5]"],"output":"[4, 6, 8]"}]}' --cnt=$CNT --out=$LOG_DIR/addElemsTwoLists.log 1> /dev/null 2> /dev/null
echo 15/26
timeout -k 1s $TIMEOUT stack exec -- hplus --json='{"query":"[Int] -> Int", "inExamples":[{"inputs":["[1, 3, 1]"],"output":"11"}]}' --cnt=$CNT --out=$LOG_DIR/sumSquares.log 1> /dev/null 2> /dev/null
echo 16/26
timeout -k 1s $TIMEOUT stack exec -- hplus --json='{"query":"[Int] -> [Int]", "inExamples":[{"inputs":["[1, 3, 2]"],"output":"[1, 2]"}]}' --cnt=$CNT --out=$LOG_DIR/removeMax.log 1> /dev/null 2> /dev/null
echo 17/26
timeout -k 1s $TIMEOUT stack exec -- hplus --json='{"query":"(Bool, Bool) -> Bool", "inExamples":[{"inputs":["(Data.Bool.True, Data.Bool.True)"],"output":"Data.Bool.False"}, {"inputs":["(Data.Bool.True, Data.Bool.False)"],"output":"Data.Bool.True"}, {"inputs":["(Data.Bool.False, Data.Bool.True)"],"output":"Data.Bool.True"}, {"inputs":["(Data.Bool.False, Data.Bool.False)"],"output":"Data.Bool.True"}]}' --cnt=$CNT --out=$LOG_DIR/nandPair.log 1> /dev/null 2> /dev/null
echo 18/26
timeout -k 1s $TIMEOUT stack exec -- hplus --json='{"query":"[Bool] -> Bool", "inExamples":[{"inputs":["[Data.Bool.False, Data.Bool.False]"],"output":"Data.Bool.True"}, {"inputs":["[Data.Bool.True, Data.Bool.False]"],"output":"Data.Bool.False"}, {"inputs":["[Data.Bool.True]"],"output":"Data.Bool.True"}]}' --cnt=$CNT --out=$LOG_DIR/allEqBool.log 1> /dev/null 2> /dev/null
echo 19/26
timeout -k 1s $TIMEOUT stack exec -- hplus --json='{"query":"[[a]] -> [[a]]", "inExamples":[{"inputs":["[[1, 3]]"],"output":"[[3, 1]]"}]}' --cnt=$CNT --out=$LOG_DIR/mapReverse.log 1> /dev/null 2> /dev/null
echo 20/26
timeout -k 1s $TIMEOUT stack exec -- hplus --json='{"query":"[Maybe a] -> Bool", "inExamples":[{"inputs":["[Data.Maybe.Nothing, Data.Maybe.Just 1]"],"output":"Data.Bool.False"}, {"inputs":["[Data.Maybe.Just 0, Data.Maybe.Just 0]"],"output":"Data.Bool.True"}, {"inputs":["[Data.Maybe.Just 0, Data.Maybe.Nothing]"],"output":"Data.Bool.False"}]}' --cnt=$CNT --out=$LOG_DIR/allJust.log 1> /dev/null 2> /dev/null
echo 21/26
timeout -k 1s $TIMEOUT stack exec -- hplus --json='{"query": "[(Bool,Bool)] -> Bool", "inExamples":[{"inputs": ["[(Data.Bool.True, Data.Bool.True), (Data.Bool.False, Data.Bool.False)]"],"output":"Data.Bool.False"}, {"inputs":["[(Data.Bool.True, Data.Bool.True), (Data.Bool.False, Data.Bool.False), (Data.Bool.True, Data.Bool.True)]"],"output":"Data.Bool.False"}, {"inputs":["[(Data.Bool.True, Data.Bool.True), (Data.Bool.True, Data.Bool.True)]"],"output":"Data.Bool.True"}, {"inputs":["[(Data.Bool.False, Data.Bool.False)]"],"output":"Data.Bool.False"}]}' --cnt=$CNT --out=$LOG_DIR/andListPairs.log
echo 22/26
timeout -k 1s $TIMEOUT stack exec -- hplus --json='{"query":"(Int, Int) -> Int", "inExamples":[{"inputs":["(1, 2)"],"output":"3"}]}' --cnt=$CNT --out=$LOG_DIR/sumPairEntries.log 1> /dev/null 2> /dev/null
echo 23/26
timeout -k 1s $TIMEOUT stack exec -- hplus --json='{"query":"(Eq a) => [(a, a)] -> [(a, a)]", "inExamples":[{"inputs":["[(1, 2), (2, 2), (3, 0)]"],"output":"[(2, 2)]"}]}' --cnt=$CNT --out=$LOG_DIR/filterPairsTyClass.log 1> /dev/null 2> /dev/null
echo 24/26
timeout -k 1s $TIMEOUT stack exec -- hplus --json='{"query":"[Float] -> [Float]", "inExamples":[{"inputs":["[1, 2, 3]"],"output":"[1.5, 2.5, 3.5]"}]}' --cnt=$CNT --out=$LOG_DIR/mapAddFloat.log 1> /dev/null 2> /dev/null
echo 25/26
timeout -k 1s $TIMEOUT stack exec -- hplus --json='{"query":"[Int] -> [Int]", "inExamples":[{"inputs":["[100, 200, 300]"],"output":"[120, 220, 320]"}]}' --cnt=$CNT --out=$LOG_DIR/mapAddLarge.log 1> /dev/null 2> /dev/null
echo 26/26
echo "Done"