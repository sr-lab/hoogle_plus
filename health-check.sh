HOOGLE_PLUS_ROOT=/home

cd $HOOGLE_PLUS_ROOT/hoogle_plus_orig/
rm -rf logs
mkdir logs
timeout -k 1s 90s stack exec -- hplus "[[[a]]] -> [a]" --cnt=5 --out=logs/flatten.log 1> /dev/null 2> /dev/null

cd $HOOGLE_PLUS_ROOT/hoogle_plus_ext/
rm -rf logs
mkdir logs
timeout -k 1s 60s stack exec -- hplus "[[[a]]] -> [a]" --cnt=5 --out=logs/flatten.log 1> /dev/null 2> /dev/null
timeout -k 1s 90s stack exec -- hplus "[Int] -> [Int]" --example="[([[1, 1, 0, 0, 1, 2]], [0, 0, 1, 2])]" --cnt=25 --out=logs/removeFirstOnes.log 1> /dev/null 2> /dev/null

cd $HOOGLE_PLUS_ROOT/hoogle_plus_examp/
rm -rf logs
mkdir logs
timeout -k 1s 90s stack exec -- hplus --json='{"query":"[Int] -> [Int]", "inExamples":[{"inputs":["[1, 1, 0, 0, 1, 2]"],"output":"[0, 0, 1, 2]"}]}' --cnt=25 --out=logs/removeFirstOnes.log 1> /dev/null 2> /dev/null
