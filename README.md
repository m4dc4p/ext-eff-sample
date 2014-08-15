This repository uses the Haskell package "extensible-effects" to define a
framework in which a file cannot be read (or written) without first being
opened in the proper mode. In fact, a program (using this framework) that
attempts to read or write a file before opening the file will not compiler.

Files in this repository include:

  * `src/OpenFile2.hs` -- Defines the primitive operations for opening files
  in read or write mode. 
  * `src/ReadFile.hs` -- Primitives for reading from a file.
  * `src/WriteFile.hs` -- Primitives for writing to a file.
  * `src/Test.hs` -- Sample code that demonstrates how to use the `ReadFile` 
  and `WriteFile` effects.
  * `src/OpenFile/hs` -- An earlier implementation of the same effects --
  superseded by `OpenFile2`

Additionally, a presentaiton on the sample can be found in `func-guild-pres.pdf`.