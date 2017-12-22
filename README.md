# foochess
A chess engine written in Haskell

## REFERENCES
[RFC3092]  Etymology of "Foo", 1 April 2001. <http://www.faqs.org/rfcs/rfc3092.html>


## PREREQUISITES

### Glasgow Haskell Compiler

You will need the Glorious Glasgow Haskell Compilation System.
You can download an installer for it from http://haskell.org.
I had good experiences downloading the Haskell platform installer
for Mac OS. Linux, OS X, and Windows are suppored.


## USAGE

Once you have haskell installed, you can build and run with
```
$ ghc -O main.hs square.hs
$ ./main
```
To compile and run the test suite use
```
$ ghc -O test.hs square.hs
$ ./test
```
## EXAMPLE
```
epiphany:foochess mark$ ./main
 bR bN bB bQ bK bB bN bR
 bP bP bP bP bP bP bP bP
 -- -- -- -- -- -- -- --
 -- -- -- -- -- -- -- --
 -- -- -- -- -- -- -- --
 -- -- -- -- -- -- -- --
 wP wP wP wP wP wP wP wP
 wR wN wB wQ wK wB wN wR
|
Welcome to Haskell Chess!
White to move; may I suggest e2e4?
move?
```







