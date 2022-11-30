LOG_DIR=bench-logs-extension
TIMEOUT=90s
CNT=35

echo "Setup..."
rm -r $LOG_DIR 1> /dev/null 2> /dev/null
mkdir $LOG_DIR 1> /dev/null 2> /dev/null
stack build 1> /dev/null 2> /dev/null
stack exec -- hplus generate --preset partialfunctions 1> /dev/null 2> /dev/null


timeout -k 1s $TIMEOUT stack exec -- hplus "[Int] -> [Int]" --example="[([[1, 2, 3]], [2, 3, 4])]" --cnt=$CNT --out=$LOG_DIR/mapAdd.log 1> /dev/null 2> /dev/null
echo 1/15
timeout -k 1s $TIMEOUT stack exec -- hplus "[Int] -> [Int]" --example="[([[1, 2]], [1, 4])]" --cnt=$CNT --out=$LOG_DIR/mapSquare.log 1> /dev/null 2> /dev/null
echo 2/15
timeout -k 1s $TIMEOUT stack exec -- hplus "[Int] -> [Int]" --example="[([[1, 2, 3]], [1, 2, 3, 4])]" --cnt=$CNT --out=$LOG_DIR/appendConst.log 1> /dev/null 2> /dev/null
echo 3/15
timeout -k 1s $TIMEOUT stack exec -- hplus "[Int] -> [Int]" --example="[([[1, 2, 3]], [1, 3])]" --cnt=$CNT --out=$LOG_DIR/filterDiff.log 1> /dev/null 2> /dev/null
echo 4/15
timeout -k 1s $TIMEOUT stack exec -- hplus "[Int] -> [Int]" --example="[([[1, 1, 0, 1, 2]], [1, 1])]" --cnt=$CNT --out=$LOG_DIR/getFirstOnes.log 1> /dev/null 2> /dev/null
echo 5/15
timeout -k 1s $TIMEOUT stack exec -- hplus "[Int] -> [Int]" --example="[([[1, 1, 0, 0, 1, 2]], [0, 0, 1, 2])]" --cnt=$CNT --out=$LOG_DIR/removeFirstOnes.log 1> /dev/null 2> /dev/null
echo 6/15
timeout -k 1s $TIMEOUT stack exec -- hplus "[Int] -> [Int] -> [Int]" --example="[([[0, 2, 4], [2, 4, 6], [2, 4])]" --cnt=$CNT --out=$LOG_DIR/listIntersect.log 1> /dev/null 2> /dev/null
echo 7/15
timeout -k 1s $TIMEOUT stack exec -- hplus "[Int] -> Int" --example="[([[1, 2, 0, 3, 0, 1]], 3)]" --cnt=$CNT --out=$LOG_DIR/indexConst.log 1> /dev/null 2> /dev/null
echo 8/15
timeout -k 1s $TIMEOUT stack exec -- hplus "[Int] -> Bool" --example="[([[2, 3, 4]], Data.Bool.True), ([[2, 1, 4]], Data.Bool.False)]" --cnt=$CNT --out=$LOG_DIR/allGreaterThan.log 1> /dev/null 2> /dev/null
echo 9/15
timeout -k 1s $TIMEOUT stack exec -- hplus "[Int] -> [Int]" --example="[([[0, 0, 4, 4, 3]], [4, 3])]" --cnt=$CNT --out=$LOG_DIR/dropConst.log 1> /dev/null 2> /dev/null
echo 10/15
timeout -k 1s $TIMEOUT stack exec -- hplus "[Int] -> [Int]" --example="[([[2, 0, 1, 3]], [2, 3])]" --cnt=$CNT --out=$LOG_DIR/filterGreaterThan.log 1> /dev/null 2> /dev/null
echo 11/15
timeout -k 1s $TIMEOUT stack exec -- hplus "[(Int, Int)] -> [(Int, Int)]" --example="[([[(1, 2), (3, 3), (4, 5)]], [(3, 3))]" --cnt=$CNT --out=$LOG_DIR/filterPairs.log 1> /dev/null 2> /dev/null
echo 12/15
timeout -k 1s $TIMEOUT stack exec -- hplus "[Int] -> [Int]" --example="[([[1, 2, 1, 3, 4, 4]], [1, 1])]" --cnt=$CNT --out=$LOG_DIR/filterEq.log 1> /dev/null 2> /dev/null
echo 13/15
timeout -k 1s $TIMEOUT stack exec -- hplus "[Int]" --example="[([], [3, 3, 3, 3, 3])]" --cnt=$CNT --out=$LOG_DIR/replicateConst.log 1> /dev/null 2> /dev/null
echo 14/15
timeout -k 1s $TIMEOUT stack exec -- hplus "[Int] -> [Int] -> [Int]" --example="[([[1, 2, 3], [3, 4, 5]], [4, 6, 8])]" --cnt=$CNT --out=$LOG_DIR/addElemsTwoLists.log 1> /dev/null 2> /dev/null
echo 15/15