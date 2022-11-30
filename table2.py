# generate the second table
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

dir1 = "bench-logs-examples"
dir2 = "bench-logs-extension"

ind = 1
for (n, t, e) in exercises:
    logfile1 = open(dir1 + "/" + n + ".log")
    logfile2 = open(dir2 + "/" + n + ".log")
    lines1 = logfile1.readlines()
    lines2 = logfile2.readlines()
    
    if len(lines1) > 1:
        time1 = round(float(lines1[1][:-1]), 2)
    else:
        time1 = "-"

    sols1 = len(lines1) // 2


    if len(lines2) > 1:
        timeTot2 = round(float(lines2[1].split()[1]), 2)
        timeMat2 = round(float(lines2[1].split()[0]), 2)
    else:
        timeTot2 = timeMat2 = "-"
    
    sols2 = len(lines2) // 2

    print(ind, "&", n, "&", time1, "&", sols1, "&", timeTot2, "&", timeMat2, "&", sols2, "\\\\")
    ind +=1

    logfile1.close()
    logfile2.close()