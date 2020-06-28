# Olox
## Introduction
Olox is an ocaml port of Java olox interpreter from the book ["crafting interpreters"](https://craftinginterpreters.com) by Bob Nystrom.
At the moment, it implements all the major parts of the lox language (not including the extra features described in the *Challenges*
sections at the end of every chapter). There are some slight differences from jlox since it relies so heavily on the Java runtime, but
most programs writen for jlox should also run with olox. If you find any exmaples that don't run, feel free to open a pull request.

## Building and Running
Before building olox, you must download the required dependencies. At the moment of writing this, they are:
- [OCaml](https://github.com/ocaml/ocaml) V4.10
- [Dune](https://github.com/ocaml/dune) V2.6
- [OUnit2](https://github.com/gildor478/ounit) V2.2 (only required for running tests)
- [ocamlformat](https://github.com/ocaml-ppx/ocamlformat) V0.14.2 (only required for formating the code)

Earlier/newer versions of any of these dependencies might work, but they are not tested. If [opam](https://github.com/ocaml/opam) is 
installed with a switch to ocaml V4.10, you can run the following command to get all the dependencies:
```
opam install dune ounit2 ocamlformat
```
To build the executable, run:
```
dune build
```
And to run the tests, run:
```
dune runtest
```
Once built, there are two options to getting to the executable. You can either use dune's exec command like this:
```
dune exec bin/main.exe (or bin/main.bc for the bytecode version of the executable)
```
Or find the main.exe executable that was build by dune in the default location: `_build/default/bin`.

## Differences from jlox
As mentioned above, olox has a few differences from the books jlox interpreter. Some of these stem from the fact that ocaml was used as the
host language instead of Java, others stem from differences in the implementation. The main differences (that I know of) are as follow:
- Many errors are not caught at compile time: jlox has a static analysis pass out of necessity, but some static checking was added since the
  pass was already there. Since olox uses a mostly imutable data structure to represent the environment of the interpretet, the static pass
  is no longer required. An unfortunate side effect is that all the checks added to the static pass of jlox are now relegated to the runtime.
- Some differences in how values are printed: The way olox prints values is significantly different to how jlox does things. While a
  program written for jlox should run with olox, there is a high likleyhood that at least some of the output is different.
- Last value printing: olox always prints the return value of the last statement in the program. jlox only does this when typing in an
  expression in the REPL. In practice this means that most programs will print out `nil` after they are done running.

There are probably many more differences. As more difference are found, I'll add them to this list.

## TODO
This interpreter is far from perfect. Since this was my first major program in ocaml, even the stuff that does work is probably not done in
an optimal way. Here is a list of tasks that I would like to takle in the future if I have time:
- [ ] Refactor the interpreter to use monads: At the moment, the interpreter (found in `olox/interpreter.ml`) has a bunch of nested match
      expressions to handle situations when many successive tasks can fail. This is the ideal place to use things like monads to flatten
      out the match pyramids.
- [ ] Add the language extensions mentions in the book: Multiple of the *Challenges* sections in the book recomend extension to the lox
      language. In a perfect world, any good lox interpreter should feature these extensions.
- [ ] Add more native functions: The only built in function in olox is `clock()`, which returns the amount of seconds since the epoch. While
      a toy language like this might not really need a substantial standard library, basic IO functions would help a lot in writing usefull
      programs in lox. (This would also make programming more complex test programs much easier)
- [ ] Fix the error messages: error messages, especially in the parser, are pretty bad. The parser does a pretty poor job of synchronizing
      after finding and error and many of it's messages are not clear or missleading. In a perfect world, as I use the lox language more, I
      would also work on making the errors as clear as possible.
- [ ] More testing: any language implementation has holes, and I'm sure that olox has more holes than average. The testing suite included with
      the code needs to be expanded and large/complex example programs should also be added as black box end-to-end tests.

It's unclear if I will ever have the time/motivation to tackle these issues, but I still think this list is helpfull for anyone looking at the
code. Instead of spending more time on this codebase, it is much more likely I will be spending time on interpreters/compilers for my own
languages (after all, that was the reason for working through the book in the first place).

## Acknowledgements
The lox language and jlox are both from the book ["crafting interpreters"](https://craftinginterpreters.com) by Bob Nystrom. The design of olox
is (very) heavily influenced by the code for jlox included in the book. If you are interested in designing/implementing interpreters, I highly
recomend reading the book.
