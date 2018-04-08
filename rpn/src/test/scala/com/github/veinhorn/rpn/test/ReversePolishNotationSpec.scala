package com.github.veinhorn.rpn.test

import com.github.veinhorn.rpn.ReversePolishNotation

/**
  * Created by VEINHORN on 06.04.2018.
  */
class ReversePolishNotationSpec extends BaseSpec {
  import ReversePolishNotation._

  it should "tokenize simple arithmetic expressions to the token lists" in {
    tokenize("1+2") should equal(List(Number("1"), Plus("+"), Number("2")))
    tokenize("3-4") should equal(List(Number("3"), Minus("-"), Number("4")))
    tokenize("5*6") should equal(List(Number("5"), Multiply("*"), Number("6")))
    tokenize("7/8") should equal(List(Number("7"), Divide("/"), Number("8")))
    tokenize("9^2") should equal(List(Number("9"), Power("^"), Number("2")))

    tokenize("9 - 10") shouldEqual List(Number("9"), Minus("-"), Number("10"))
    tokenize("(11 * 12)") shouldEqual List(OpenBracket("("), Number("11"), Multiply("*"), Number("12"), CloseBracket(")"))
  }

  it should "tokenize complex arithmetic expressions to the token lists" in {
    tokenize("(1+2) * 3") shouldEqual ("(" ~> "1" ~> "+" ~> "2" ~> ")" ~> "*" ~> "3").tokens
    tokenize("((1+2) * 3)") shouldEqual ("(" ~> "(" ~> "1" ~> "+" ~> "2" ~> ")" ~> "*" ~> "3" ~> ")").tokens
  }

  it should "transform a simple arithmetic expressions to the list of tokens in RPN postfix notation" in {
    rpn("1+2") shouldEqual "1 2 +".rpn
    rpn("3-4") shouldEqual "3 4 -".rpn
    rpn("5*6") shouldEqual "5 6 *".rpn
    rpn("7/8") shouldEqual "7 8 /".rpn
    rpn("9^2") shouldEqual "9 2 ^".rpn
  }

  it should "evaluate simple arithmetic expressions" in {
    eval("1 + 2") shouldEqual 3
    eval("4 - 3") shouldEqual 1
    eval("5 * 6") shouldEqual 30
    eval("16 / 4") shouldEqual 4
    eval("2 ^ 4") shouldEqual 16
  }

  it should "evaluate complex arithmetic expressions" in {
    eval("(1 + 2) * 3") shouldEqual 9
  }

  it should "evaluate functions" in {
    eval("min(1, 3)") shouldEqual 1
    eval("min(3, 1)") shouldEqual 1

    eval("max(4, 2)") shouldEqual 4
    eval("max(2, 4)") shouldEqual 4
  }
}
