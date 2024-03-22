# Timelog

* Project Name: Python-like Functional Programming Language with Intergrated Type-System 
* Name: Maxwell Wraith-Whiting
* Student Reference Number: 2560800
* Supervisor: Simon Fowler

## Guidance

* This file contains the time log for your project. It will be submitted along with your final dissertation.
* **YOU MUST KEEP THIS UP TO DATE AND UNDER VERSION CONTROL.**
* This timelog should be filled out honestly, regularly (daily) and accurately. It is for *your* benefit.
* Follow the structure provided, grouping time by weeks.  Quantise time to the half hour.

## Week 1

### 19 September 2023

* *1 hour* Initial meeting with S. Fowler (project reassignment)
* *2 hour* Research on programming languages to prepare for writing new project proposal.

### 21 September 2023

* *0.5 hour* Researching programming laguage concepts of significance

### 22 September 2023

* *1 hour* Meeting to discuss new proposal with S. Fowler
* *2 hour* Research on type systems and ambigious parsing

## Week 2

### 29 September 2023

* *1 hour* Meeting to solidify proposal: A programming language with a sophisticated type system (Hindley-Milner or approx.)

## Week 3

### 2nd October 2023

* *3 hour* Reading material relating to parametric polymorphism implementations
* *4 hour* Researching formal syntax defintions and working on implementing a rudimentary scanner and parser

### 3rd October 2023

* *2 hour* Researching Type systems papers
* *1.5 hour* Designing basic syntax and grammar

### 4th October 2023

* *3 hours* Researching lambda calculus

### 5th October 2023

* *1.5 hours* Subject research

### 6th October 2023

* *1.5 hours* Researching SKI combinators
* *0.5 hours* Trying to figure out why I don't understand SKI combinators :(

### 7th October 2023

* *1.5 hours* Subject research

## Week 4

### 9th October 2023

* *1.5 hours* Subject research

### 10th October 2023

* *1.5 hours* Subject research

### 11th October 2023

* *1.5 hours* Subject research

### 12th October 2023

* *1.5 hours* Reading about "B, C, K, W system" and why it's important

### 13th October 2023

* *1.5 hours* "Re"-Researching SKI combinators

## Week 5

### 16th October 2023

* *1.5 hours* Researching intermediate language representations in lambda calculus 

### 17th October 2023

* *3 hours* Researching G-machines

### 20th October 2023
    
* *1 hour* Designing a minimal inital Token specification

## Week 6

### 26th October 2023

* *3.0 hour* Designed basic AST for an easily typecheck intermediate language
* *3.0 hour* Began implementing the IR AST

### 29th October 2023

* *0.5 hour* Outlined inference steps
* *2.5 hour* Figured out Ocaml Signatures structures
* *1.5 hour* Implemented a signature for a language construct of a simply typed lambda calculus

## Week 7

### 30th October 2023

* *0.5 hour* Tried and failed to write a basic unification algorithm. (Need more understanding and research.)
* *1.5 hour* Looked over Hindley-Milner type inference rules (which I still don't understand fully).
* *4.0 hour* Spent time reading the type-systems literature on dynamic typing with and without annotations

### 1st November 2023

* *0.5 hour* Tried to rename modules in Ocaml's Dune build system. (Failed. It broke everything and Dune isn't explaining itself well.)
* *3.0 hour* Scoured the Dune documentation trying to figure out why it breaks on certain file names...

### 2nd November 2023

* *3.5 hour* Began implementing Quantifiers as a concept to the unfinished extended lambda calculus typechecker. 
* *0.5 hour* Outlined the valid tokens in extended lambda calculus. 
* *1.0 hour* Struggled valiantly with Ocaml's ability to "let () = Anything that doesn't return stuff, side-effects included", (Great success was had.) 

## Week 8

### 6th November 2023

* *1.0 hour* Reading.
* *2.5 hour* Reading other things.

### 9th November 2023

* *1.5 hour* After much study (and pain), removed signatures from the code due to complete lack of use in their given context.
* *4.0 hour* Implemented a basic type check for the baisic lambda calculus. (THE MOST / LEAST PRODUCTIVE 4 HOURS I'VE EVER HAD.)
* *1.0 hour* Introduced type-annotations to the typechecker (because things are not working without them yet...)

### 12nd November 2023

* *0.5 hour* Researched Hindley-Milner's original implementation rules.
* *1.0 hour* Changed focus to language frontend in Ocaml, using Menhir and Ocamlex
* *2.5 hour* Worked on the parser.

## Week 9

### 16th November 2023

* *3.5 hour* Began an ill-fated attempt to implement the generation of fresh type variables into the unfinished type-checker.
* *1.0 hour* Began undoing the damage of the last 3 hours leaving all of 80 lines of the changes actually added to the project.

### 18th November 2023

* *2.5 hour* Learned how Ocaml tables work and added looking up IDs in a table to the parser. 

## Week 10

### 20th November 2023

* *1.0 hour* Pretty printing for the base types of the lambda calculus.

### 23rd November 2023

* *2.5 hour* Added the idea of a type constraint via the new Constraint Module
* *2.0 hour* Added constructors to build the AST directly from the Parser.

## Week 11

### 29th November 2023

* *2.0 hour* Exteneded Lexer and did some major house-keeping of the project files.

## Week 12

### 4th December 2023

* *3.5 hour* Created HM_rules module for specifically handling the unification, generalisation, and instantiation of polytype variables.


### 10th December 2023

* *1.0 hour* Added pair constructors
* *1.5 hour*

## Week 13

### 12th December 2023

* *1.0 hour* Integrated all the disparate parts of the type checker and fully implemented it for the extended lambda calculus IR.
* *1.0 hour* Lot's of house-keeping. (Refactoring etc.)

## Week 14

### 19th December 2023

* *1.0 hour* Added debug AST printer 

### 20th December 2023

* *1.5 hour* Made a far better AST printer like Unix tree command. (Feels like Ocaml has finally clicked!)

## Week 17

### 11st January 2024

* *2.5 hour* Frontend Parser created

### 13rd January 2024

* *2.0 hour* Minor restructuring
* *2.5 hour* Dune is being a pain
* *1.5 hour* Fixed buggy "use package" statements in dissertation template.

## Week 18

### 18th January 2024

* *2.0 hour* Frontend work
* *1.0 hour* Fixing parser
* *2.0 hour* IR float issue reappeared. Fixed it.

### 19th January 2024

* *1.5 hour* Code clean up.

### 20th January 2024

* *0.5 hour* IR Converter planning.
* *1.0 hour* More, IR Converter designing.
* *1.0 hour* Type inference rules research.

## Week 19

### 22nd January 2024

* *2.5 hour* Recursive typechecking rule, tentatively added.
* *2.0 hour* AST now pretty prints on execution.
* *0.5 hour* Busy work.

### 24th January 2024

* *4.0 hour* Let-rec rule works now.
* *1.0 hour* Let-rec rule doesn't work now.
* *2.0 hour* Let-rec rule is back to working again, just correctly this time.

### 25th January 2024

* *1.5 hour* I honestly don't know. I was coding.
* *1.5 hour* The REPL code is somewhat tidier.

### 27th January 2024

* *3.5 hour* Research day. Collating theoretical underpinnings of type inference from the simply typed lambda calculus to ML.
* *3.0 hour* Research.
* *3.5 hour* Triple research.

## Week 20

### 29th January 2024

* *0.5 hour* Researching tree-walk interpreters.
* *1.0 hour* Pipeline structured.
* *4.0 hour* Skeleton of the interpreter.

### 30th January 2024

* *3.0 hour* Added if evaluation to interpreter.
* *2.0 hour* Added runtime errors and did some research on runtime error detection in functional programming languages.

### 31st January 2024

* *2.0 hour* Bug-fixing.

## Week 21

### 5th February 2024

* *0.5 hour* Actually converted my time log from pen and paper to this file.
* *2.0 hour* REPL now can handle exceptions better.
* *1.0 hour* Improved AST outputs for IR.

### 10th February 2024

* *2.5 hour* Removed defunct features.

## Week 22

### 12nd February 2024

* *1.0 hour* Added entry type for env elements in interpreter.
* *0.5 hour* Vref type for heap allocated types.
* *0.5 hour* Verbose logging added poorly.
* *2.5 hour* Fully implemented function application.
* *0.5 hour* Value system now works.

### 13rd February 2024

* *4.0 hour* Major tidy up of main module.
* *2.5 hour* Miscellaneous programming tasks for project upkeep. 

### 15th February 2024

* *4.0 hour* Dissertation update and added list concatenation to the interpreter.

## Week 23

### 21st February 2024

* *2.0 hour* IR converter is functioning.
* *3.5 hour* Conversion rules for basic operation.
* *1.0 hour* Elif expressions are in.
* *2.0 hour* Trouble with whitespace indentation.

## Week 24

### 27th February 2024

* *1.0 hour* Bash tidy up for easy running.
* *1.0 hour* Dissertation update, largely MoSCoW stuff.

### 28th February 2024

* *3.5 hour* Global scope definitions.

## Week 25

### 5th March 2024

* *1.0 hour* Fixed Cons to actually be Cons. 
* *2.0 hour* Modified Unification algorithm for TypeList.
* *2.5 hour* Unary operation support in type checker.

### 9th March 2024

* *2.0 hour* OpUnary now fully supported.
* *3.5 hour* Cons List operation now fully supported.

### 10th March 2024

* *2.0 hour* Debugging nasty application / env issues.
* *0.5 hour* Patched the bug with a dirty hack, environment is now not actually immutable...
* *1.5 hour* Temporary fix.

## Week 26

### 12nd March 2024

* *2.0 hour* Frontend linkage to function calls.

### 15th March 2024

* *1.0 hour* Modulo, which is poorly defined in the programming world. (Remainder is also called modulo which... *why?*)
* *3.5 hour* Dissertation update, syntax tables.

### 17th March 2024

* *3.5 hour* Fixed interpreter from dirty mutable env hack back to passed env.
* *2.0 hour* Closures now work again.
* *3.5 hour* Dissertation update, I think its getting somewhere.

## Week 27

### 18th March 2024

* *3.5 hour* Writing
* *2.5 hour* Still Writing

### 19th March 2024

* *1.0 hour* Writing
* *0.5 hour* Writing

### 20th March 2024

* *2.5 hour* Writing
* *1.0 hour* Writing
* *1.5 hour* Endless writing

### 21st March 2024

* *1.5 hour* Editing
* *2.5 hour* Presentation prep.
* *7.0 hour* Finalising dissertation.