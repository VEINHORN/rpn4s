package com.github.veinhorn.rpn.test

import org.scalatest.{FlatSpec, Matchers}
import com.github.veinhorn.rpn.ReversePolishNotation

/**
  * Created by VEINHORN on 06.04.2018.
  */
class ReversePolishNotationSpec extends FlatSpec with Matchers {
  import ReversePolishNotation._

  it should "tokenize string expression into a list of tokens" in {
    // числа
    tokenize("1+2") should equal (List(Number("1"), Plus("+"), Number("2")))
    // операторы
  }

  it should "transform a list of tokens into a list of tokens in RPN (postfix form)" in {
    rpn("1+2") should equal (List(Number("1"), Number("2"), Plus("+")))
  }
}
