# Octogulf

Octogulf is a scripting language (a programming language) currently being developed. It is a project
in programming language design not actual programming language *implementation* but a working interpreter will
at least be provided, no matter how inefficient the interpreter will end up being. 

## Core features / values
* Prefix notation
* Support for anonymous functions
* Support for OOP-like programming

Come back later to find out more and see how it develops. 

## First look

This is just an exempt from the test suite:

```octogulf
Main {
  AEQ(1 1)

  \= BASIC ARITHMETIC \
   \= INTEGER \
   AEQ(+1 9 10)
   AEQ(+_1 9 8)
   AEQ(-5 3 2)
   AEQ(-5 _3 8)

  \= STRING OPERATORS \
   AEQ(+"Hi " "there" "Hi there")

  \= 42 means all tests passed \
  ValDump(42)
}
```
