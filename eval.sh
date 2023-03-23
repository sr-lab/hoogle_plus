FLAG_SET_1=S1
FLAG_SET_2=S2

if (test $# -ne 0 && test $# -ne 1) || (test $# -eq 1 && test $1 != $FLAG_SET_1 && test $1 != $FLAG_SET_2) 
then 
    echo "Usage: bash $0 [$FLAG_SET_1|$FLAG_SET_2]"
    exit 1
fi

if test $# -eq 0 || (test $# -eq 1 && test $1 = $FLAG_SET_1)
then 
    echo "44 benchmark on original Hoogle+"
    cd hoogle_plus_orig/
    sh eval_orig.sh $*
    cd ..
fi
if test $# -eq 0 || (test $# -eq 1 && test $1 = $FLAG_SET_2)
then
    echo "26 benchmark on original Hoogle+ with examples"
    cd hoogle_plus_examp/
    sh eval_examp.sh $*
    cd ..
fi

echo "59 benchmark on the extension"
cd hoogle_plus_ext
sh eval_ext.sh $*
cd ..

python3 ./results.py