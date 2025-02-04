# A C Compiler I'm working on for fun right now

This is supposed to turn into a better version of my distributed programming
language, I will be using llvm for the backend connections and slowly begin
implementing more language features as I go along. Currently working on this
during the end of my days for fun, making good progress. Hopefully I can
bootstrap this thing if it works, although IDK if I really want to reimplement
the entirety of the language again in another langague.

## Lessons Learned from this project

* Wrap objects using unique_ptr
* Use more polymorphism in codebase
* Split up datastructures used throughout the codebase (IE: AST Tree) into their
  own files / namespaces.
* Need to incorproate a checker / validator for my statements

## Important Node

I need to implment proper C++ memory handling, especially with my usage of
pointers to heap allocated memory. Will implment this sometime in the near
future, likely with unique_ptr.

- [ ] Proper memory management (smart pointers)

## Lexing

- [ ] Error Handling

## Parsing

- [ ] Extern functions 

- [x] return statement

- [ ] Error Handling w/ tips

- [ ] Variable Assignment / Mutability

- [ ] Arrays

- [ ] Access operator (testing.value for structs)

- [x] Expression

- [x] Binary Operator

- [x] Operator Precedence

- [x] Function Call

- [x] Variable Declerations

- [x] Struct Decleration

- [x] Function Decleration

### Primitives
- [x] Int

- [ ] Int32

- [ ] Int64

- [x] Float

- [x] Boolean

- [ ] String => Will represent this as a primitive
