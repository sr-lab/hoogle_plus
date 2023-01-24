# generates a latex table with the results of the benchmarks
# expects the results to be in the bench-logs-extension

import time

exercises = [ "firstRight", "firstKey", "flatten", "repl-funcs", "containsEdge", 
    "multiApp", "appendN", "pipe", "intToBS", "cartProduct", "applyNtimes", 
    "firstMatch", "mbElem", "mapEither", "hoogle01", "zipWithResult", "splitStr",
    "lookup", "fromFirstMaybes", "map", "maybe", "rights", "mbAppFirst", 
    "mergeEither", "test", "multiAppPair", "splitAtFirst", "2partApp", "areEq", 
    "eitherTriple", "mapMaybes", "head-rest", "appBoth", "applyPair", 
    "resolveEither", "head-tail", "indexesOf", "app3", "both", "takeNdropM", 
    "firstMaybe", "mbToEither", "pred-match", "singleList",
    "mapAdd", "mapSquare", "appendConst", "filterDiff", "getFirstOnes",
    "removeFirstOnes", "listIntersect", "indexConst", "allGreaterThan", 
    "dropConst", "filterGreaterThan", "filterPairs", "filterEq", 
    "replicateConst", "addElemsTwoLists"]

dir1 = "hoogle_plus_ext/logs"
dir2 = "hoogle_plus_orig/logs"
dir3 = "hoogle_plus_examp/logs"

print("\\documentclass{article}")
print("\\begin{document}")
print("\\begin{table}")
print("\\centering")
print("\\caption{Results of the first set of benchmarks (collected ", time.asctime() ,"). Table 3 in the paper.}", sep='')
print("\\begin{tabular}{rl||rr||rr}")
print("\\hline ")
print(" & & \\multicolumn{2}{c||}{\\textsc{Hoogle+}} & \\multicolumn{2}{c}{Our Extension} \\\\")    
print("\\# & Benchmark & Time (s) & Sols. & Time (s) & Sols. \\\\ \\hline")

ind = 0
for n in exercises:
    ind +=1
    try:
        logfileE = open(dir1 + "/" + n + ".log")
        logfileO = open(dir2 + "/" + n + ".log")
    except:
        continue

    linesE = logfileE.readlines()
    linesO = logfileO.readlines()
    
    solsE, solsO = len(linesE) // 2, len(linesO) // 2
    timeE = timeO = '-'

    if len(linesO) > 1:
        timesO = linesO[1].split()
        if len(timesO) == 1:
            timeO = round(float(timesO[0]), 2)
        else:
            raise ValueError('Expected one real number')

    if len(linesE) > 1:
        timesE = linesE[1].split()
        if len(timesE) == 2:
            timeE = round(float(timesE[1]), 2)
        else:
            raise ValueError('Expected two real numbers')
    
    print(ind, "&", n, "&", timeO, "&", solsO, "&", timeE, "&", solsE, "\\\\")
    
    logfileO.close()
    logfileE.close()

print("\\hline ")
print("\\end{tabular}")
print("\end{table}")
print("\\begin{table}")
print("\\centering")
print("\\caption{Results of the second set of benchmarks (collected ", time.asctime() ,"). Table 4 in the paper.}", sep='')
print("\\begin{tabular}{rl||rr||rrr}")
print("\\hline ")
print(" & & \\multicolumn{2}{c||}{\\textsc{Hoogle+}} & \\multicolumn{3}{c}{Our Extension} \\\\")    
print("\\# & Benchmark & Time (s) & Sols. & Time (s) & Unify & Sols. \\\\ \\hline")
print("\\hline ")

ind = 0
for n in exercises:
    ind +=1
    try:
        logfileE = open(dir1 + "/" + n + ".log")
        logfileO = open(dir3 + "/" + n + ".log")
    except:
        continue

    linesE = logfileE.readlines()
    linesO = logfileO.readlines()
    
    solsE, solsO = len(linesE) // 2, len(linesO) // 2
    matchE = timeE = timeO = '-'

    if len(linesO) > 1:
        timesO = linesO[1].split()
        if len(timesO) == 1:
            timeO = round(float(timesO[0]), 2)
        else:
            raise ValueError('Expected two real numbers')

    if len(linesE) > 1:
        timesE = linesE[1].split()
        if len(timesE) == 2:
            timeE = round(float(timesE[1]), 2)
            matchE = round(float(timesE[0]), 2)
        else:
            raise ValueError('Expected two real numbers')
    
    print(ind, "&", n, "&", timeO, "&", solsO, "&", timeE, "&", matchE, "&", solsE, "\\\\")
    
    logfileO.close()
    logfileE.close()

print("\\hline ")
print("\\end{tabular}")
print("\\end{table}")
print("\\end{document}")