# Project Paradigms (Module 8) Project - Almost Vaction (AMV) language
The AMV language is implemented in Haskell. Therefore you need to have
[Stack](https://haskellstack.org/) installed.

## Setup
* Clone this project/ unzip it, and run the following commands in your
  preferred command line/ terminal:
* `stack build`

### Stack Dependencies
* [leonschoorl/sprockell](https://github.com/leonschoorl/sprockell) from Github
  commit: [`b9d26ba` (June 5 2019)](https://github.com/leonschoorl/sprockell/tree/b9d26ba27bf4f008f37e863c79338465100a426a)
* [parsec](http://hackage.haskell.org/package/parsec)

## Get a working compiler
* `stack exec amv` (This will ask for input)
* `stack exec -- amv filename.amv` (Run the compiler with a specified file name,
  it will take only the first argument in consideration)

## Test
* `stack test`

## Project files
```shell
amv-lang/                 # → Root of the AMV Language
├── app/                  # → app directory
│   ├── Compiler.hs       # → The AMV compiler which asks for a file to compile
├── src/                  # → The src directory with all the files for frontend and backend of the compiler
│   ├── Parser.hs         # → File for the parser frontend
|   test/                 # → The test directory
│   ├── Parser.hs         # → Test the parser
├── .gitignore            # → The .gitignore file
├── LICENSE               # → The License file
├── package.yaml          # → File needed for Stack that generates cabal file
├── README.md             # → This README file
├── Setup.hs              # → File that can be needed for Cabal installation, but not used with Stack
└── stack.yaml            # → File to specify the Haskell version, and gives pointers where to download Sprockell from.
```
