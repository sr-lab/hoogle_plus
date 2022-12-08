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
    "firstMaybe", "mbToEither", "pred-match", "singleList"
    "mapAdd", "mapSquare", "appendConst", "filterDiff", "getFirstOnes",
    "removeFirstOnes", "listIntersect", "indexConst", "allGreaterThan", 
    "dropConst", "filterGreaterThan", "filterPairs", "filterEq", 
    "replicateConst", "addElemsTwoLists"]

dir = "bench-logs-extension"

print("\\documentclass{article}")
print("\\begin{document}")
print("\\begin{table}")
print("\\centering")
print("\\caption{Results (collected ", time.asctime() ,").}", sep='')
print("\\begin{tabular}{rl||rrr}")
print("\\hline ")
print("\\# & Benchmark & Time (s) & Unify(s) & Sols\\\\")
print("\\hline ")

ind = 0
for n in exercises:
    ind +=1
    try:
        logfile = open(dir + "/" + n + ".log")
    except:
        continue

    lines = logfile.readlines()
    
    if len(lines) > 1:
        timeTot2 = round(float(lines[1].split()[1]), 2)
        timeMat2 = round(float(lines[1].split()[0]), 2)
    else:
        timeTot2 = timeMat2 = "-"
    
    sols2 = len(lines) // 2

    print(ind, "&", n, "&", timeTot2, "&", timeMat2, "&", sols2, "\\\\")
    
    logfile.close()

print("\\hline ")
print("\\end{tabular}")
print("\\end{table}")
print("\\end{document}")