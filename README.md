# Extension to hoogle_plus

Type-driven, component based synthesis, showcasing TYpe Guided Abstract Refinement (TYGAR), extended with support for generation of constants and lambda-abstractions.

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
3. Build
3.1. Clone this repository
3.2. Compile
	```
	cd hoogle_plus
	git checkout origin/match
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

- TYGARQB: Bounded abstraction refinement. Use `stack exec -- hplus --stop-refine=True --stop-threshold=10 "<query>"`. Replace `10` with any number. This is the maximum refinements HooglePlus will make.


## Evaluation
In the evaluation presented in the paper, we compare the extension to two versions of Hoogle+:
- The 44 benchmarks that do not require input-output examples use the original version of Hoogle+
- The 15 benchamrks that require input-output examples use a version of Hoogle+ that support examples

Execute the following steps:
1. Run the 59 benchmarks on the extension
   ```
   git checkout match
   sh eval_ext.sh
   ```
2. Run the 44 benchmarks on the original hoogle plus
   ```
   git checkout hplus-original-bench
   sh eval_orig.sh
   ```
3. Run the 15 benchmarks on the version of hoogle plus that supports examples
   ```
   git checkout hplus-examples-bench
   stack install hoogle && hoogle generate
   sh eval_examp.sh
   ```

TODO: uniformizar hoogle plus, +, H, ...; fazer eval_orig.sh; remover este readme


[vscode-remote]: <https://code.visualstudio.com/docs/remote/containers>