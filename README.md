# Extension to Hoogle+
We extended the Hoogle+ program synthesizer for Haskell, so that it supports the generation of constants and input-output examples.

# Setup

## System Prerequisites
- Python2
- Git

## Instalation

  

1. Install z3 version 4.8.1:
	```
	sudo apt-get install -y git build-essential python
	git clone https://github.com/Z3Prover/z3.git
	cd z3
	git checkout z3-4.8.1
	python scripts/mk_make.py
	cd build
	make
	make install
	```
2. Install haskell stack tool
	```
	sudo apt-get install -y libtinfo-dev zlib1g-dev haskell-stack
	stack upgrade
	```
3. Clone this repository

4. Build
	```
	cd hoogle_plus
	git checkout origin/master
	stack build
	```

# Usage

## Build the database

First, we have to choose a component set and build a database. The widest component set is `partialfunctions`, and contains the 294 components described in the paper.

```
stack exec -- hplus generate --preset partialfunctions
```

## Queries

Once the databse is built, we can do queries. For instance, the following query searches for a function whose type is `Maybe a -> [a] -> a`:

```
stack exec -- hplus "Maybe a -> [a] -> a"
```

However, to synthesize constants and lambda-abstractions, which are the contributions of this extension, examples must be supplied. For instance, the following query provides an example (with a single argument) that maps the list `[1, 2, 3]` to the list `[2, 3, 4]`. We also require the Petri net to synthesize 35 solutions (`--cnt=35`), because many of them will be eliminated while generating constants and lambda abstractions, as described in the paper.
```
stack exec -- hplus "[Int] -> [Int]" --example="[([[1, 2, 3]], [2, 3, 4])]" --cnt=35
```
The default search mode is `TYGARAQ` as described in the paper: unbounded abstraction refinement.
You may try searches with different variants with the following command line args:

- TYGARQ: Unbounded abstraction refinement. This is the default mode. The initial abstract cover are the types in the query.  

- TYGARQ0: Unbounded abstraction refinmenet. The initial abstract cover is empty. Use `stack exec -- hplus --use-refine=tygar0 "<query>"`

- NOGAR: No refinement. Use `stack exec -- hplus --use-refine=nogar "<query>"`

- TYGARQB: Bounded abstraction refinement. Use `stack exec -- hplus --stop-refine=True --stop-threshold=10 "<query>"`. Replace `10` with any number. This is the maximum refinements Hoogle+ will make.


## Evaluation

In the evaluation presented in the paper, we compare the extension to two versions of Hoogle+:

- The 44 benchmarks that do not require input-output examples are compared to the original version of Hoogle+

- The 15 benchamrks that require input-output examples are compared to a version of Hoogle+ that support examples

To run the 59 benchmarks on the extension (it should take 66 minutes aprox.), do:
```
git checkout origin/master
sh eval_ext.sh
```
For each benchmark, a log file is created in the directory `bench-logs-extension`.
To run the benchamarks in Hoogle+, you first need to clone the its original repository:
```
git clone https://github.com/TyGuS/hoogle_plus.git
```
To run the 44 benchmarks on the original Hoogle+, run [`eval_orig.sh`](eval_orig.sh):
```
cd hoogle_plus
git checkout origin/master
sh eval_orig.sh
```
For each benchmark, a log file is created in the directory `bench-logs-original`.
To run the 15 benchmarks that have examples, run [`eval_examp.sh`](eval_examp.sh):
```
cd hoogle_plus
git checkout origin/oopsla20_revision
stack install hoogle && hoogle generate
sh eval_examp.sh
```
For each benchmark, a log file is created in the directory `bench-logs-examples`.