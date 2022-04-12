# haskell-on-the-beach
Some of my haskell projects from 2nd year in uni

Our uni coursework in 2nd semester was to design our own programming language using Alex and Happy, and Haskell for backend behaviour.
As a teamwork project, for some of the files I have less contribution - Read.hs, TTLParser.hs; for some - more - Grammar.y, Tokens.x.
The interpreter of the language is entirely written by me.

In its essence, it is a query language that reads turtle files and outputs triples according to different queries.
If you want to run it on your own machine, make sure you have Alex and Happy installed (cabal install alex/happy).
Run alex Tokens.x and happy Grammar.y in the same directory. Compile Stql.hs (ghc Stql.hs).
Now you can pass files written in the language (Stql.exe rickroll.stql).

Using the foo.ttl file, you can query things like:
READ foo WHERE foo.OBJ < 50
READ foo WHERE OBJ < 50 (same output, syntax sugar)
READ foo WHERE IF (foo.OBJ < 50 AND foo.OBJ > 20) THEN CHANGE SUBJ foo.PRED OBJ+1 AND INSERT foo.SUBJ PRED false END

etc. etc.

A lot can be said about different syntax sugar, error information, static type checking. I'm pretty happy with the project given the time and workload we have.
