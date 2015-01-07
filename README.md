40 Points
==========
A functional (like the paradigm) Scala implementation of the Chinese card game
[40 Points](http://en.wikipedia.org/wiki/Sheng_Ji) (known by many other names -- see link).

Currently, this game can be played in the terminal and supports a single deck of cards. 
This isn't quite where we want to be, since:
- it's not very pretty
- you can see everyone else's cards
- everyone has to be in front of the same computer
- you have to type the cards you want to play

The next planned step is to implement a web frontend and plug this game in the back.

As far as I know, this game hasn't been implemented in English (not sure about in Chinese, either).

Demo
----
TODO

Rules
-----
Please consult the [Wikipedia page](http://en.wikipedia.org/wiki/Sheng_Ji) for the game rules.

Development
===========

Packages
---------
```
├── main
    └── scala
        ├── card -- self contained playing card library
        └── game -- core game logic, game and monad utilities
            ├── command -- handle user input and error statuses via command pattern
            ├── console -- things for the terminal-version only
            └── play -- computing things related to hand strengths
```

Playing the game
-----------------
Install [sbt](http://www.scala-sbt.org/) and [Scala](http://scala-lang.org/).

```
sbt run
```

Compiling and Testing
--------
Respectively:
```
sbt compile
```

```
sbt test
```

Contributing
------------
No licenses yet, but message me if you're interested in contributing!

