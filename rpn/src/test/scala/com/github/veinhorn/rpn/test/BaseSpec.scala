package com.github.veinhorn.rpn.test

import com.github.veinhorn.rpn.ReversePolishNotation._
import org.scalatest.{FlatSpec, Matchers}

/**
  * Some implicits to simplify testing
  */
class BaseSpec extends FlatSpec with Matchers {

  val toToken: String => Token = {
    case e if e.forall(c => c.isDigit || c == '.') => Number(e)
    case "+" => Plus("+")
    case "-" => Minus("-")
    case "*" => Multiply("*")
    case "/" => Divide("/")
    case "^" => Power("^")

    case "(" => OpenBracket("(")
    case ")" => CloseBracket(")")
  }

  implicit class RpmExpression(expression: String) {
    def rpn: List[Token] = expression.split(" ").map(toToken).toList

    def ~>(token: String): List[Token] = toToken(expression) :: toToken(token) :: Nil
  }

  implicit class ListWrapper(val tokens: List[Token]) {
    def ~>(token: String): ListWrapper = ListWrapper(tokens :+ toToken(token))
  }
}
