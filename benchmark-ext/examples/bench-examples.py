#!/usr/bin/env python3

import subprocess

exercises = [
    ("[Int] -> [Int]", "[([[1, 2, 3]], [2, 3, 4])]")
]

def exercise_command (ex):
    return ['stack', 'exec', '--', 'hplus',  ex[0], f'--example={ex[1]}', '--cnt=100']

# main code
for ex in exercises:
    proc = subprocess.run(exercise_command(ex), capture_output=True, text=True, timeout=60000)
    print(proc.stdout)