## hackasm
An assembler for the Hack assemble language from the Nand2Tetris
coursera course. 

Relevant course: [project 6](https://www.nand2tetris.org/project06)

Notes on the assembler:
- Is a 3 pass assembler
  - 1 pass to build symbol table for loops
  - 1 pass to build symbol table for addresses
  - 1 pass to convert instructions into the internal haskell representation
- The assembler makes use of the [attoparsec](http://hackage.haskell.org/package/attoparsec) applicative parser combinator in the parsing stage
- It uses [binary](http://hackage.haskell.org/package/binary) library in to spit out the binary data for the assembler
- By default the assembler spits out a string of 1s and 0s to create the
correctly formatted strings for the project

## Instructions
Assumes the reader has [stack](https://docs.haskellstack.org/en/stable/README/) installed on their system. 

Build the program with:

```
stack build
```

To view intermediate representation

```
stack exec hackasm analyse <input>
```

To assemble actual files:

```
stack exec hackasm <input> <output>
```

To generate a binary file instead of a bunch of strings:

```
stack exec hackasm --binary <input> <output> 
```
