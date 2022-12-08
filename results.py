import time

exercises = [
    ("mapAdd", "[Int] -> [Int]", "[([[1, 2, 3]], [2, 3, 4])]"),
    ("mapSquare", "[Int] -> [Int]", "[([[1, 2, 3]], [1, 4, 9])]"),
    ("appendConst", "[Int] -> [Int]", "[([[1, 2, 3]], [1, 2, 3, 4])]"),
    ("filterDiff", "[Int] -> [Int]", "[([[1, 2, 3]], [1, 3])]"),
    ("getFirstOnes", "[Int] -> [Int]", "[([[1, 1, 0, 1, 2]], [1, 1])]"),
    ("removeFirstOnes", "[Int] -> [Int]", "[([[1, 1, 0, 0, 1, 2]], [0, 1, 2])]"),
    ("listIntersect", "[Int] -> [Int] -> [Int]", "[([[0, 2, 4], [2, 4, 6], [2, 4])]"),
    ("indexConst", "[Int] -> Int", "[([[1, 2, 0, 3, 0, 1]], 3)]"),
    ("allGreaterThan", "[Int] -> Bool", "[([[2, 3, 4]], Data.Bool.True), ([[2, 1, 4]], Data.Bool.False)]"),
    ("dropConst", "[Int] -> [Int]", "[([[0, 0, 4, 4, 3]], [4, 3])]"),
    ("filterGreaterThan", "[Int] -> [Int]", "[([[2, 0, 1, 3]], [2, 3])]"),
    ("filterPairs", "[(Int, Int)] -> [(Int, Int)]", "[([[(1, 2), (3, 3), (4, 5)]], [(3, 3))]"),
    ("filterEq", "[Int] -> [Int]", "[([[1, 2, 1, 3, 4, 4]], [1, 1])]"),
    ("replicateConst", "[Int]", "[([], [3, 3, 3, 3, 3])]"),
    ("addElemsTwoLists", "[Int] -> [Int] -> [Int]", "[([[1, 2, 3], [3, 4, 5]], [4, 6, 8])]")
]

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
for (n, t, e) in exercises:
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