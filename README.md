# rpn-scala

[rpn-scala](https://github.com/VEINHORN/scala-rpn) is a lightweight Scala library which helps you to start developing your own calculator. :smile: It's based on [reverse polish notation](https://en.wikipedia.org/wiki/Reverse_Polish_notation) and [Shunting-yard algorithm](https://en.wikipedia.org/wiki/Shunting-yard_algorithm) algorithms.

Key features:

* operator priorities
* support functions and inner functions

Check out [tests](https://github.com/VEINHORN/scala-rpn/blob/master/rpn/src/test/scala/com/github/veinhorn/rpn/test/ReversePolishNotationSpec.scala) for more use cases.

## Functions

The library supports different functions such as `sin`, `cos`, `min`, `max`, etc. You also can provide your own function by [PR](https://github.com/VEINHORN/scala-rpn/pulls).

## Installation

To grab the library just put this lines to your `build.sbt`:

```scala
resolvers += Resolver.bintrayRepo("veinhorn", "maven")

"com.github.veinhorn" %% "rpn-core" % "0.0.1"
```
