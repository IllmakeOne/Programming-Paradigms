# Project Paradigms (Module 8) Project - Almost Vaction (AMv) language
The AMv language is implemented in Haskell. Therefore you need to have
[Stack](https://haskellstack.org/) installed. It will run on the latest
Haskell 8.0.2 (lts-9.21 on Stackage) as we used that version also during FP.

## Setup
* Clone this project/ unzip it, and run the following commands in your
  preferred command line/ terminal:
* `stack build`

### Stack Dependencies
* [leonschoorl/sprockell](https://github.com/leonschoorl/sprockell) from Github  
  Modified so now the directory is imported directly by Stack. (See report for
    details)
* [parsec](http://hackage.haskell.org/package/parsec)  
  Will be downloaded.

## Get a working compiler
* `stack exec amv`: This will print the usage information
* `stack exec -- amv c <file> <amountOfThreads>`: run a given file with the
  specified amount of threads. Example: `stack exec -- amv c examples/peterson.amv 3`
* `stack exec -- amv ast <file>`: print the AST of the given file
* `stack exec -- amv gen <file> <amountOfThreads>`: print the Spril Code of the
  given file
* `stack exec -- amv write <file> <amountOfThreads>`: write the Spril Code of the
  given file, in the same directory as <file>.spril.

## Tests
Currently the automated tests will run all the front-end tests.
* `stack test`: should say: `PP-AMV-Lang-x.x.x.x: Test suite amv-test passed.`
* For the code generation and explanations: see the report.  

## Project files
```shell
amv-lang/                 # → Root of the AMv Language
├── app/                  # → app directory
│   ├── Compiler.hs       # → The AMV compiler which asks for a file to compile
├── examples/             # → The directory with all our example files and generated Spril code.
├── sprockell/            # → The sprockell directory, as we modified sprockell. Will be automatically imported by Stack.
├── src/                  # → The src directory with all the files for frontend and backend of the compiler
│   ├── BasicParsers.hs   # → File for the parser frontend with all the parsers from Parsec
│   ├── Generator.hs      # → The code generator
│   ├── Parser.hs         # → File for the parser frontend
│   ├── Structure.hs      # → The structure for the AST
│   ├── TreeWalker.hs     # → File for the frontend of the compiler
│   ├── TreeWalkerTests.hs # → File for the frontend otests
|   test/                 # → The test directory
│   ├── Tests .hs         # → Test the frontend with stack
├── LICENSE               # → The License file
├── package.yaml          # → File needed for Stack that generates cabal file
├── README.md             # → This README file
├── Setup.hs              # → File that can be needed for Cabal installation, but not used with Stack
└── stack.yaml            # → File to specify the Haskell version, and gives pointers where to download Sprockell from.
```
