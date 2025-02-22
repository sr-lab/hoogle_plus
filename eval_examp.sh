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
echo 1/15
timeout -k 1s $TIMEOUT stack exec -- hplus --json='{"query":"[Int] -> [Int]", "inExamples":[{"inputs":["[1, 2]"],"output":"[1, 4]"}]}' --cnt=$CNT --out=$LOG_DIR/mapSquare.log 1> /dev/null 2> /dev/null
echo 2/15
timeout -k 1s $TIMEOUT stack exec -- hplus --json='{"query":"[Int] -> [Int]", "inExamples":[{"inputs":["[1, 2, 3]"],"output":"[1, 2, 3, 4]"}]}' --cnt=$CNT --out=$LOG_DIR/appendConst.log 1> /dev/null 2> /dev/null
echo 3/15
timeout -k 1s $TIMEOUT stack exec -- hplus --json='{"query":"[Int] -> [Int]", "inExamples":[{"inputs":["[1, 2, 3]"],"output":"[1, 3]"}]}' --cnt=$CNT --out=$LOG_DIR/filterDiff.log 1> /dev/null 2> /dev/null
echo 4/15
timeout -k 1s $TIMEOUT stack exec -- hplus --json='{"query":"[Int] -> [Int]", "inExamples":[{"inputs":["[1, 1, 0, 1, 2]"],"output":"[1, 1]"}]}' --cnt=$CNT --out=$LOG_DIR/getFirstOnes.log 1> /dev/null 2> /dev/null
echo 5/15
timeout -k 1s $TIMEOUT stack exec -- hplus --json='{"query":"[Int] -> [Int]", "inExamples":[{"inputs":["[1, 1, 0, 0, 1, 2]"],"output":"[0, 0, 1, 2]"}]}' --cnt=$CNT --out=$LOG_DIR/removeFirstOnes.log 1> /dev/null 2> /dev/null
echo 6/15
timeout -k 1s $TIMEOUT stack exec -- hplus --json='{"query":"[Int] -> [Int] -> [Int]", "inExamples":[{"inputs":["[0, 2, 4]","[2, 4, 6"],"output":"[2, 4]"}]}' --cnt=$CNT --out=$LOG_DIR/listIntersect.log 1> /dev/null 2> /dev/null
echo 7/15
timeout -k 1s $TIMEOUT stack exec -- hplus --json='{"query":"[Int] -> Int", "inExamples":[{"inputs":["[1, 2, 0, 3, 0, 1]"],"output":"3"}]}' --cnt=$CNT --out=$LOG_DIR/indexConst.log 1> /dev/null 2> /dev/null
echo 8/15
timeout -k 1s $TIMEOUT stack exec -- hplus --json='{"query":"[Int] -> Bool", "inExamples":[{"inputs":["[2, 3, 4]"],"output":"Data.Bool.True"},{"inputs":["[2, 1, 4]"],"output":"Data.Bool.False"}]}' --cnt=$CNT --out=$LOG_DIR/allGreaterThan.log 1> /dev/null 2> /dev/null
echo 9/15
timeout -k 1s $TIMEOUT stack exec -- hplus --json='{"query":"[Int] -> [Int]", "inExamples":[{"inputs":["[0, 0, 4, 4, 3]"],"output":"[4, 3]"}]}' --cnt=$CNT --out=$LOG_DIR/dropConst.log 1> /dev/null 2> /dev/null
echo 10/15
timeout -k 1s $TIMEOUT stack exec -- hplus --json='{"query":"[Int] -> [Int]", "inExamples":[{"inputs":["[2, 0, 1, 3]"],"output":"[2, 3]"}]}' --cnt=$CNT --out=$LOG_DIR/filterGreaterThan.log 1> /dev/null 2> /dev/null
echo 11/15
timeout -k 1s $TIMEOUT stack exec -- hplus --json='{"query":"[(Int, Int)] -> [(Int, Int)]", "inExamples":[{"inputs":["[(1, 2), (3, 3), (4, 5)]"],"output":"[(3, 3)"}]}' --cnt=$CNT --out=$LOG_DIR/filterPairs.log 1> /dev/null 2> /dev/null
echo 12/15
timeout -k 1s $TIMEOUT stack exec -- hplus --json='{"query":"[Int] -> [Int]", "inExamples":[{"inputs":["[1, 2, 1, 3, 4, 4]"],"output":"[1, 1]"}]}' --cnt=$CNT --out=$LOG_DIR/filterEq.log 1> /dev/null 2> /dev/null
echo 13/15
timeout -k 1s $TIMEOUT stack exec -- hplus --json='{"query":"[Int]", "inExamples":[{"inputs":[],"output":"[3, 3, 3, 3, 3]"}]}' --cnt=$CNT --out=$LOG_DIR/replicateConst.log 1> /dev/null 2> /dev/null
echo 14/15
timeout -k 1s $TIMEOUT stack exec -- hplus --json='{"query":"[Int] -> [Int] -> [Int]", "inExamples":[{"inputs":["[1, 2, 3]","[3, 4, 5]"],"output":"[4, 6, 8]"}]}' --cnt=$CNT --out=$LOG_DIR/addElemsTwoLists.log 1> /dev/null 2> /dev/null
echo 15/15
echo "Done"