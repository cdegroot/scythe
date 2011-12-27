Please ignore, nothing to see. 

SCala Yummy THrift Engine
=========================

An experiment to see whether a Scala based thrift compiler/code generator can generate
code closer to Scala-ness. I'm not extending the Thrift C++ compiler because, frankly,
I can't be bothered. 


Caveat
======

I want to do this cleanly. Therefore, I don't generate code but generate from the Thrift
AST a Scala AST which I print with the tools in the scala-refactoring library. 

However, the Maven artifacts for that library don't seem to be very clean, so I'm relying
on a local installation. See build.sbt for details. 

