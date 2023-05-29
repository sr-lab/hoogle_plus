This README contains information about the contents of this docker image.

# Overview

* This **Docker image** contains the artifact of the paper 
  > Henrique Botelho Guerra, João F. Ferreira, and João Costa Seco, Hoogle⋆: Constants and λ-abstractions in 
Petri-net-based Synthesis using Symbolic Execution, in 37th European Conference on Object-Oriented Programming (ECOOP 2023), LIPIcs, Vol.~263, 2023. <https://doi.org/10.4230/LIPIcs.ECOOP.2023.38>
* The image contains:
  - the **Haskell source code of Hoogle⋆**, the extension of Hoogle+, which is already compiled and ready to use;
  - a script to run the experiments described in Section 5 of the paper, generating tables 3 and 4 of the paper;
  - and, for evaluation purposes, it also includes the two versions of Hoogle (the original one, and the version that supports input-output examples) already compiled and ready to use. 

* The artifact is functional, reusable, and available.

## Artifact’s functionality

In our paper, we propose an extension to the Hoogle+ program synthesizer so that it synthesizes solutions with constants and lambda abstractions. The artifact contains the implementation of the extension and allows each one to run benchmarks and see the code.

* The artifact is *consistent*: as it implements the extension to Hoogle+, described in the paper, and generates tables 3 and 4 of Section 5 of the paper, which summarize the evaluation.

* The artifact is *complete*: it contains the extension made to Hoogle+, described in Section 4 of the paper, whose source code is present in the folders `/home/hoogle_plus_ext/app` and `/home/hoogle_plus_ext/src`. The unification algorithm, described in Section 3 of the paper, is implemented in the folder `/home/hoogle_plus_ext/src/SymbolicMatch`.

* The artifact is *exercisable*: Tables 3 and 4 of the paper can be generated by running the script, as explained soon.
  
* The artifact is *documented*. The code is commented and we map each algorithm on the paper to a function/module in the artifact:
  
| Algorithm | Source code | 
| ----------- | ----------- |
| Algorithm 1 | function eval in `/home/hoogle_plus_ext/src/SymbolicMatch/Eval.hs` |
| Algorithm 2 | functions main and executeSearch in `/home/hoogle_plus_ext/app/HooglePlus.hs` |
| Algorithm 3 | functions executeCheck and runExampleChecks in `/home/hoogle_plus_ext/src/HooglePlus/GHCChecker.hs`, and the function that converts to Haskell notation corresponds to showExpr, in `/home/hoogle_plus_ext/src/SymbolicMatch/Expr.hs` |
| Algorithm 4 | function synthLamba `/home/hoogle_plus_ext/src/HooglePlus/GHCChecker.hs` and function linearSynth in `/home/hoogle_plus_ext/src/HooglePlus/LinearSynth.hs` |
| Algorithm 5 | function completeExpr in `/home/hoogle_plus_ext/src/HooglePlus/LinearSynth.hs` |
| Algorithm 6 | function applyMatch in `/home/hoogle_plus_ext/src/HooglePlus/LinearSynth.hs` |
| The addition of the wildcard component | is done by function generateEnv, in `/home/hoogle_plus_ext/src/Database/Environment.hs` |
| The unification algorithm | function match (`/home/hoogle_plus_ext/src/SymbolicMatch/Match.hs`); each case of this function is related to an inference rule presented in the paper. |


## Artifact’s reusability
To change the way the occurrences of the wildcard component are replaced, simply redefine the function runExampleChecks (`/home/hoogle_plus_ext/src/HooglePlus/GHCChecker.hs`).

## Artifact’s availability
The artifact is on [DARTS](https://drops.dagstuhl.de/opus/institut_darts.php).

## Artifact Requirements
Only `docker` is required.

# Getting Started
## Load the Docker image

```
docker load < hoogle-star_latest.gz
docker run -it hoogle-star:latest
```

## Structure
* The artifact includes, already compiled and ready to use:
  - the extension to the Hoogle+, Hoogle⋆, in folder `/home/hoogle_plus_ext/`;
  - the original version of Hoogle+, in folder `/home/hoogle_plus_orig/`;
  - the original version of Hoogle+, in folder `/home/hoogle_plus_examp/`;
* It also includes the script `/home/eval.sh`, to run the evaluation.

## The script `/home/eval.sh` 
The script `/home/eval.sh` runs the evaluation done in section 5 of the paper, and generates tables 3 and 4. In detail, it does the following:
  * It runs both Hoogle⋆ and the original version of Hoogle+ in the first set of 44 benchmarks, which do not require examples;
  * It runs both Hoogle⋆ and the version of Hoogle+ that supports examples in the second set of 26 benchmarks, which use input-output examples;
  * It takes 2 hours approx; to run only the second set of 26 benchmarks (which is the most important set because its benchmarks require the generation of constants and lambda-abstractions), pass S2 as an argument (`sh eval.sh S2`), and this should take 80 minutes;
  * It prints a LaTeX document in the `stdout`.

## Specific scripts
The scripts `/home/hoogle_plus_ext/eval_ext.hs`, `/home/hoogle_plus_examp/eval_examp.hs` and `/home/hoogle_plus_orig/eval_orig.hs` can be used separately to run the benchmarks on specific versions.         
  - Each one creates a folder `logs` (for instance, the `eval_ext.sh` creates a folder `/home/hoogle_plus_ext/logs`), containing a log file for each benchmark (with the synthesized solutions and times). The arguments `S2` and `S1` can be used in script `/home/hoogle_plus_ext/eval_ext.hs`, to select one of both sets of benchmarks.
  - For an even shorter evaluation, run Hoogle⋆ the 26 benchmarks that take input-output examples (approx. 40 minutes). This is the most relevant set of benchmarks, as most of them require the generation of constants and lambda abstractions, which is the key contribution of our work.
    ```
    cd /home/hoogle_plus_ext
    sh eval_ext.sh S2
    ```
  - Each log file contains, for each solution, a line with the output and a line with the times. In the case of Hoogle+ and Hoogle+ with examples, there is a single time: the total time spent. In the case of Hoogle⋆, the time spent replacing wildcards is also provided, and this is the first value of the line.
  
* To run a problem in the extension, `cd /home/hoogle_plus_ext/` and use `stack exec`. For instance, if the goal is to synthesize a function `[Int] -> [Int]` that adds 1 to each element of the input list, we can supply the input-output example `[([[1, 2, 3]], [2, 3, 4])]`:
    ```
    cd /home/hoogle_plus_ext
    stack exec -- hplus "[Int] -> [Int]" --example="[([[1, 2, 3]], [2, 3, 4])]" --cnt=35
    ```
    Note that the argument `--cnt` specifies the number of incomplete functions that should be generated, and we have used `35` in benchmarks that require input-output examples, as explained in the paper.

* To run the previous problem in the version of Hoogle+ with examples:
    ```
    cd /home/hoogle_plus_examp
    stack exec -- hplus --json='{"query":"[Int] -> [Int]", "inExamples":[{"inputs":["[1, 2, 3]"],"output":"[2, 3, 4]"}]}' --cnt=35
    ```
* The run original version of Hoogle+ does not support examples, so we only can provide a query type:
    ```
    cd /home/hoogle_plus_orig
    stack exec -- hplus "[Int] -> [Int]" --cnt=35
    ```

## The script `/home/health-check.sh` 
We provide a script (`/home/health-check.sh`), that runs a smaller, faster subset of benchmarks to check whether everything is working well. It takes 5 minutes: it runs Hoogle+ and Hoogle* on benchmark 3 and runs Hoogle+ with examples and Hoogle* on benchmark 50. We have chosen these benchmarks because they are all solved by Hoogle+ and Hoogle*. To run the script:

```
cd /home
sh health-check.sh
```
The results are in the following log files:
| Benchmark | Tool | Log file| Solutions in AMD 5600G 16 GB Ubuntu 22.04, inside docker |
| ----------- | ----------- |----------- | ------|
| `flatten` | Hoogle+ | `hoogle_plus_orig/logs/flatten.log`| 5|
| `flatten` | Hoogle* | `hoogle_plus_ext/logs/flatten.log`| 5 |
| `removeFirstOnes` | Hoogle+ with examples | `hoogle_plus_examp/logs/removeFirstOnes.log`| 1 |
| `removeFirstOnes` | Hoogle*| `hoogle_plus_ext/logs/removeFirstOnes.log`| 12 |
  
Note that in the log files, each solution has two lines: the synthesized function and the time(s).