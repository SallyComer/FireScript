# FireScript
A Haskell-powered scripting language that has built-in support for concurrency.

The interpreter can be used as a REPL if invoked with no arguments.

A multi-line statement or declaration can be entered by using
the ':{' and ':}' commands, much like in GHCI.

Although variables are reassignable, data is immutable (aside from 'Embers')

Classes can be defined, but inheritance must be done manually (for now):
    * construct an instance of the desired parent class(es)
    * use the 'merge' function to merge two objects' variables and methods


Modules are supported


'Import' loads the given file, and merges that environment with the current one.
