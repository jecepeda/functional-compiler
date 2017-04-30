# functional-compiler

This repository is one case study in the subject "Programación declarativa" taught at UCLM

The objectives of the case study are:
- Write an interpreter
- Use Haskell to write the interpreter

### How to compile the project?
1. First, install [Stack](https://docs.haskellstack.org/en/stable/README/)
2. Then, clone this repository
  *  `git clone https://github.com/JCepedaVillamayor/funcional-compiler.git`
3. Go to the project folder and build the project
  * `cd <project-path>`
  * `stack build`

### What tools have been used?

- The lexer has been implemented using [Alex](https://www.haskell.org/alex/), which is a tool for generating lexical analyzers in Haskell. This tool is similar to the tool [Flex](https://github.com/westes/flex) and [Jflex](http://www.jflex.de/). A tutorial showing how to use alex can be found [here](https://www.haskell.org/alex/doc/alex.pdf)
- The syntax parser has benn implemented using [Happy](https://www.haskell.org/happy/). Happy is a parser generator system: it takes a file containing a BNF specification and produces a Haskell containing a parser for the grammar. A tutorial showing how to use Happy can be found [here](https://www.haskell.org/happy/doc/happy.pdf)

### Which language do we want to describe?

The BNF syntax for the grammar can be found [here](src/grammar_specification.ebnf)

### ¿How to run the code?

Once the project is built, you need to execute the following command:
  * `stack exec functional-compiler-exe`

