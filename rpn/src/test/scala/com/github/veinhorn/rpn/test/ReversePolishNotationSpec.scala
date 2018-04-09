package com.github.veinhorn.rpn.test

import com.github.veinhorn.rpn.ReversePolishNotation

import scala.math.BigDecimal.RoundingMode

/**
  * Created by VEINHORN on 06.04.2018.
  */
class ReversePolishNotationSpec extends BaseSpec {
  import ReversePolishNotation._

  it should "correctly tokenize floating number" in {
    tokenize("1.2345").head.asInstanceOf[Number] should equal (Number("1.2345"))
  }

  it should "tokenize simple arithmetic expressions to the token lists" in {
    tokenize("1+2") shouldEqual ("1" ~> "+" ~> "2").tokens
    tokenize("3-4") shouldEqual("3" ~> "-" ~> "4").tokens
    tokenize("5*6") shouldEqual("5" ~> "*" ~> "6").tokens
    tokenize("7/8") shouldEqual("7" ~> "/" ~> "8").tokens
    tokenize("9^2") shouldEqual("9" ~> "^" ~> "2").tokens

    tokenize("9 - 10") shouldEqual ("9" ~> "-" ~> "10").tokens
    tokenize("(11 * 12)") shouldEqual ("(" ~> "11" ~> "*" ~> "12" ~> ")").tokens
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

    eval("1.2+2.4") should equal (BigDecimal("3.6"))
  }

  it should "evaluate complex arithmetic expressions" in {
    eval("(1 + 2) * 3") shouldEqual 9
  }

  it should "evaluate functions" in {
    eval("sin(1)").setScale(5, RoundingMode.DOWN) shouldEqual BigDecimal("0.84147")
    eval("cos(1)").setScale(5, RoundingMode.DOWN) shouldEqual BigDecimal("0.54030")

    eval("min(1, 3)") shouldEqual 1
    eval("min(3, 1)") shouldEqual 1

    eval("max(4, 2)") shouldEqual 4
    eval("max(2, 4)") shouldEqual 4

    eval("min(6,5)+min(5,4)") shouldEqual 9
    eval("max(2,3)*max(4,5)") shouldEqual 15

    eval("max(max(1,2), 3) * max(4 ,max(5, 6))") shouldEqual 18
    eval("min(10, min(9, 8))^min(min(7,6), 5)") shouldEqual 32768
  }
}
