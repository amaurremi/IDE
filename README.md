# Implementation of the IDE algorithm

The goal of this project is to make the Interprocedural Distributive Environment (IDE) [1] data-flow analysis algorithm accessible in WALA.

[1] Mooly Sagiv, Thomas Reps, and Susan Horwitz. [Precise interprocedural dataflow analysis with applications to constant propagation](http://www.sciencedirect.com/science/article/pii/0304397596000722). Theoretical Computer Science, 1996.

The code of the analysis is written in [Scala](http://www.scala-lang.org/). Our analysis relies on [WALA](http://wala.sourceforge.net/wiki/index.php/Main_Page), a library for static analysis on Java bytecode written in Java. To facilitate the usage of WALA in Scala, you can use the [WALAFacade](https://github.com/cos/WALAFacade) library.

## Set Up

1. Install the WALA library into your local [Maven](http://maven.apache.org/) repository, as described in the [first](https://github.com/cos/WALAFacade#steps) step of the WALAFacade installation instructions.

2. Build the project with [SBT](http://www.scala-sbt.org/): 
  - Checkout the IDE project and navigate into its directory.
  - [Install](http://www.scala-sbt.org/release/docs/Getting-Started/Setup) SBT on your machine.
  - Navigate into the checked out IDE project directory from the command line.
    - `sbt gen-idea`, if you'd like to use [IntelliJ IDEA](http://www.jetbrains.com/idea/),
    - `sbt eclipse`, if you'd like to use [Eclipse](http://www.eclipse.org/),
    - `sbt`, if you prefer using another IDE.

## IFDS

IDE is a generalization of the IFDS algorithm [2]. Any IFDS problem can be transformed to an equivalent IDE problem and solved with the IDE solver.
Using [`IdeFromIfdsBuilder`](https://github.com/amaurremi/IDE/blob/master/src/main/ca/uwaterloo/ide/conversion/IdeFromIfdsBuilder.scala), you can solve any existing WALA IFDS problem with IDE (see `ReachingDefsIdeSpec` [example](https://github.com/amaurremi/IDE/blob/master/src/test/ca/uwaterloo/ide/example/reachingDefs/ReachingDefsIdeSpec.scala)).

[2] Thomas Reps, Susan Horwitz, and Mooly Sagiv. [Precise interprocedural dataflow analysis via graph reachability](http://dl.acm.org/citation.cfm?id=199462). Principles of Programming Languages, 1995.

## Note

This is the initial version of the implementation. Work on making the analysis more efficient and adding examples of IDE problem implementations is in progress.
