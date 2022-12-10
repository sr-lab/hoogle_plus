FLAG44=44
FLAG15=15

if (test $# -ne 0 && test $# -ne 1) || (test $# -eq 1 && test $1 -ne $FLAG44 && test $1 -ne $FLAG15) 
then 
    echo "Usage: bash $0 [$FLAG44|$FLAG15]"
    exit 1
fi

if test $# -eq 0 || (test $# -eq 1 && test $1 -eq $FLAG44)
then 
    echo "44 benchmark on original Hoogle+"
    cd hoogle_plus_orig/
    sh eval_orig.sh $*
    cd ..
fi
if test $# -eq 0 || (test $# -eq 1 && test $1 -eq $FLAG15)
then
    echo "15 benchmark on original Hoogle+ with examples"
    cd hoogle_plus_examp/
    sh eval_examp.sh $*
    cd ..
fi

echo "59 benchmark on the extension"
cd hoogle_plus_ext
sh eval_ext.sh $*
cd ..

python3 ./results.py