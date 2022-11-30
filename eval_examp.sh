LOG_DIR=bench-logs-examples
TIMEOUT=90s
CNT=35

echo "Setup..."
rm -r $LOG_DIR
mkdir $LOG_DIR
stack build
stack exec -- hplus generate --preset partialfunctions

echo "Benchmarking..."
timeout $TIMEOUT stack exec -- hplus --json='{"query":"[Int] -> [Int]", "inExamples":[{"inputs":["[1, 2, 3]"],"output":"[2, 3, 4]"}]}' --cnt=$CNT --out=$LOG_DIR/mapAdd.log  1> /dev/null 2> /dev/null
echo timeout $TIMEOUT stack exec -- hplus --json='{"query":"[Int] -> [Int]", "inExamples":[{"inputs":["[1, 2, 3]"],"output":"[2, 3, 4]"}]}' --cnt=$CNT --out=$LOG_DIR/mapAdd.log 1> /dev/null 2> /dev/null
timeout $TIMEOUT stack exec -- hplus --json='{"query":"[Int] -> [Int]", "inExamples":[{"inputs":["[1, 2]"],"output":"[1, 4]"}]}' --cnt=$CNT --out=$LOG_DIR/mapSquare.log 1> /dev/null 2> /dev/null
echo timeout $TIMEOUT stack exec -- hplus --json='{"query":"[Int] -> [Int]", "inExamples":[{"inputs":["[1, 2]"],"output":"[1, 4]"}]}' --cnt=$CNT --out=$LOG_DIR/mapSquare.log 1> /dev/null 2> /dev/null
timeout $TIMEOUT stack exec -- hplus --json='{"query":"[Int] -> [Int]", "inExamples":[{"inputs":["[1, 2, 3]"],"output":"[1, 2, 3, 4]"}]}' --cnt=$CNT --out=$LOG_DIR/appendConst.log 1> /dev/null 2> /dev/null
echo timeout $TIMEOUT stack exec -- hplus --json='{"query":"[Int] -> [Int]", "inExamples":[{"inputs":["[1, 2, 3]"],"output":"[1, 2, 3, 4]"}]}' --cnt=$CNT --out=$LOG_DIR/appendConst.log 1> /dev/null 2> /dev/null
timeout $TIMEOUT stack exec -- hplus --json='{"query":"[Int] -> [Int]", "inExamples":[{"inputs":["[1, 2, 3]"],"output":"[1, 3]"}]}' --cnt=$CNT --out=$LOG_DIR/filterDiff.log 1> /dev/null 2> /dev/null
echo timeout $TIMEOUT stack exec -- hplus --json='{"query":"[Int] -> [Int]", "inExamples":[{"inputs":["[1, 2, 3]"],"output":"[1, 3]"}]}' --cnt=$CNT --out=$LOG_DIR/filterDiff.log 1> /dev/null 2> /dev/null
timeout $TIMEOUT stack exec -- hplus --json='{"query":"[Int] -> [Int]", "inExamples":[{"inputs":["[1, 1, 0, 1, 2]"],"output":"[1, 1]"}]}' --cnt=$CNT --out=$LOG_DIR/getFirstOnes.log 1> /dev/null 2> /dev/null
echo timeout $TIMEOUT stack exec -- hplus --json='{"query":"[Int] -> [Int]", "inExamples":[{"inputs":["[1, 1, 0, 1, 2]"],"output":"[1, 1]"}]}' --cnt=$CNT --out=$LOG_DIR/getFirstOnes.log 1> /dev/null 2> /dev/null
timeout $TIMEOUT stack exec -- hplus --json='{"query":"[Int] -> [Int]", "inExamples":[{"inputs":["[1, 1, 0, 0, 1, 2]"],"output":"[0, 0, 1, 2]"}]}' --cnt=$CNT --out=$LOG_DIR/removeFirstOnes.log 1> /dev/null 2> /dev/null
echo timeout $TIMEOUT stack exec -- hplus --json='{"query":"[Int] -> [Int]", "inExamples":[{"inputs":["[1, 1, 0, 0, 1, 2]"],"output":"[0, 0, 1, 2]"}]}' --cnt=$CNT --out=$LOG_DIR/removeFirstOnes.log 1> /dev/null 2> /dev/null
timeout $TIMEOUT stack exec -- hplus --json='{"query":"[Int] -> [Int] -> [Int]", "inExamples":[{"inputs":["[0, 2, 4]","[2, 4, 6"],"output":"[2, 4]"}]}' --cnt=$CNT --out=$LOG_DIR/listIntersect.log 1> /dev/null 2> /dev/null
echo timeout $TIMEOUT stack exec -- hplus --json='{"query":"[Int] -> [Int] -> [Int]", "inExamples":[{"inputs":["[0, 2, 4]","[2, 4, 6"],"output":"[2, 4]"}]}' --cnt=$CNT --out=$LOG_DIR/listIntersect.log 1> /dev/null 2> /dev/null
timeout $TIMEOUT stack exec -- hplus --json='{"query":"[Int] -> Int", "inExamples":[{"inputs":["[1, 2, 0, 3, 0, 1]"],"output":"3"}]}' --cnt=$CNT --out=$LOG_DIR/indexConst.log 1> /dev/null 2> /dev/null
echo timeout $TIMEOUT stack exec -- hplus --json='{"query":"[Int] -> Int", "inExamples":[{"inputs":["[1, 2, 0, 3, 0, 1]"],"output":"3"}]}' --cnt=$CNT --out=$LOG_DIR/indexConst.log 1> /dev/null 2> /dev/null
timeout $TIMEOUT stack exec -- hplus --json='{"query":"[Int] -> Bool", "inExamples":[{"inputs":["[2, 3, 4]"],"output":"Data.Bool.True"},{"inputs":["[2, 1, 4]"],"output":"Data.Bool.False"}]}' --cnt=$CNT --out=$LOG_DIR/allGreaterThan.log 1> /dev/null 2> /dev/null
echo timeout $TIMEOUT stack exec -- hplus --json='{"query":"[Int] -> Bool", "inExamples":[{"inputs":["[2, 3, 4]"],"output":"Data.Bool.True"},{"inputs":["[2, 1, 4]"],"output":"Data.Bool.False"}]}' --cnt=$CNT --out=$LOG_DIR/allGreaterThan.log 1> /dev/null 2> /dev/null
timeout $TIMEOUT stack exec -- hplus --json='{"query":"[Int] -> [Int]", "inExamples":[{"inputs":["[0, 0, 4, 4, 3]"],"output":"[4, 3]"}]}' --cnt=$CNT --out=$LOG_DIR/dropConst.log 1> /dev/null 2> /dev/null
echo timeout $TIMEOUT stack exec -- hplus --json='{"query":"[Int] -> [Int]", "inExamples":[{"inputs":["[0, 0, 4, 4, 3]"],"output":"[4, 3]"}]}' --cnt=$CNT --out=$LOG_DIR/dropConst.log 1> /dev/null 2> /dev/null
timeout $TIMEOUT stack exec -- hplus --json='{"query":"[Int] -> [Int]", "inExamples":[{"inputs":["[2, 0, 1, 3]"],"output":"[2, 3]"}]}' --cnt=$CNT --out=$LOG_DIR/filterGreaterThan.log 1> /dev/null 2> /dev/null
echo timeout $TIMEOUT stack exec -- hplus --json='{"query":"[Int] -> [Int]", "inExamples":[{"inputs":["[2, 0, 1, 3]"],"output":"[2, 3]"}]}' --cnt=$CNT --out=$LOG_DIR/filterGreaterThan.log 1> /dev/null 2> /dev/null
timeout $TIMEOUT stack exec -- hplus --json='{"query":"[(Int, Int)] -> [(Int, Int)]", "inExamples":[{"inputs":["[(1, 2), (3, 3), (4, 5)]"],"output":"[(3, 3)"}]}' --cnt=$CNT --out=$LOG_DIR/filterPairs.log 1> /dev/null 2> /dev/null
echo timeout $TIMEOUT stack exec -- hplus --json='{"query":"[(Int, Int)] -> [(Int, Int)]", "inExamples":[{"inputs":["[(1, 2), (3, 3), (4, 5)]"],"output":"[(3, 3)"}]}' --cnt=$CNT --out=$LOG_DIR/filterPairs.log 1> /dev/null 2> /dev/null
timeout $TIMEOUT stack exec -- hplus --json='{"query":"[Int] -> [Int]", "inExamples":[{"inputs":["[1, 2, 1, 3, 4, 4]"],"output":"[1, 1]"}]}' --cnt=$CNT --out=$LOG_DIR/filterEq.log 1> /dev/null 2> /dev/null
echo timeout $TIMEOUT stack exec -- hplus --json='{"query":"[Int] -> [Int]", "inExamples":[{"inputs":["[1, 2, 1, 3, 4, 4]"],"output":"[1, 1]"}]}' --cnt=$CNT --out=$LOG_DIR/filterEq.log 1> /dev/null 2> /dev/null
timeout $TIMEOUT stack exec -- hplus --json='{"query":"[Int]", "inExamples":[{"inputs":[],"output":"[3, 3, 3, 3, 3]"}]}' --cnt=$CNT --out=$LOG_DIR/replicateConst.log 1> /dev/null 2> /dev/null
echo timeout $TIMEOUT stack exec -- hplus --json='{"query":"[Int]", "inExamples":[{"inputs":[],"output":"[3, 3, 3, 3, 3]"}]}' --cnt=$CNT --out=$LOG_DIR/replicateConst.log 1> /dev/null 2> /dev/null
timeout $TIMEOUT stack exec -- hplus --json='{"query":"[Int] -> [Int] -> [Int]", "inExamples":[{"inputs":["[1, 2, 3]","[3, 4, 5]"],"output":"[4, 6, 8]"}]}' --cnt=$CNT --out=$LOG_DIR/addElemsTwoLists.log 1> /dev/null 2> /dev/null
echo timeout $TIMEOUT stack exec -- hplus --json='{"query":"[Int] -> [Int] -> [Int]", "inExamples":[{"inputs":["[1, 2, 3]","[3, 4, 5]"],"output":"[4, 6, 8]"}]}' --cnt=$CNT --out=$LOG_DIR/addElemsTwoLists.log 1> /dev/null 2> /dev/null