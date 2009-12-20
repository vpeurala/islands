ISLANDS
-------

The purpose of this project is to build a tool for detecting unused code in large codebases. Initially we are planning to support Java (in bytecode form) and XML, but our plan is to build the tool in a programming language agnostic way, so that "plugins" for other languages could be added easily.

The algorithm goes as follows:
1) Declare a set of "root files", for example Main.class, web.xml or applicationConfig.xml.
2) From these roots, parse all class references and find the corresponding classes. Then parse these and continue recursively until a tree of all class dependencies is built.
3) The classes which are not in the finished tree (those not referenced by any class in the tree) are "code islands", i.e. dead code. The tool will output a list of those for easy removal.

A sample command line could be like this (just an example, this can change):
> islands -roots target/classes/fi/reaktor/hours/Main.class target/webapp/WEB-INF/web.xml -classpath target/classes
where -roots specifies the roots of the class tree, and -classpath is the root directory under which all classes are located.

RUNNING
-------
You currently must install package utf8-string to run the program. Later we can look into ways to create a proper build for this project, including all needed dependencies.

Install utf8-string with the following command:

cabal install utf8-string

