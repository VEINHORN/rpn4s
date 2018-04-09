package com.github.veinhorn.rpn

import scala.collection.immutable.Stack
import scala.math._

/**
  * Created by VEINHORN on 06.04.2018.
  */
object ReversePolishNotation {

  trait Token {
    def priority: Int = 0
    def <=(that: Token): Boolean = priority <= that.priority
  }

  case class Number(value: String) extends Token {
    override def toString: String = value
  }

  // operators and there priorities
  trait Operator extends Token
  class Plus extends Operator {
    override def priority: Int = 1
    override def toString: String = "+"
  }
  class Minus extends Operator {
    override def priority: Int = 1
    override def toString: String = "-"
  }
  class Multiply extends Operator {
    override def priority: Int = 2
  }
  class Divide extends Operator {
    override def priority: Int = 2
  }
  class Power extends Operator {
    override def priority: Int = 3
  }

  // some other symbols
  class OpenBracket extends Token
  class CloseBracket extends Token
  class Comma extends Token
  class Dot extends Token

  // functions
  trait Func extends Token {
    override def priority: Int = 4
  }
  class Sin extends Func
  class Cos extends Func
  class Min extends Func
  class Max extends Func

  /**
    * Transforms string arithmetic expression to list of tokens
    * @param expression is a string arithmetic expression
    * @return a list of tokens
    */
  def tokenize(expression: String): List[Token] = {
    /** Determines is it number or function name */
    def numberOrFunction(s: String): Token = {
      /** Tries to find supported functions */
      def getFunction(funcName: String) = funcName match {
        case "sin" => new Sin
        case "cos" => new Cos
        case "min" => new Min
        case "max" => new Max
        case _     => throw new Exception(s"Unsupported function name $s")
      }

      s.forall(c => c.isDigit || c == '.') match {
        case true => Number(s)
        case _    => getFunction(s)
      }
    }

    /** Get operator based on symbol */
    def operator(symbol: Char): Token = symbol match {
      case '+' => new Plus
      case '-' => new Minus
      case '*' => new Multiply
      case '/' => new Divide
      case '^' => new Power

      case '(' => new OpenBracket
      case ')' => new CloseBracket

      case _   => throw new Exception(s"Unsupported operator $symbol")
    }

    val parser: ((List[Token], String), Char) => (List[Token], String) = {
      // handle spaces
      case (meta, ' ') => meta
      // parse numbers
      case ((tokens, stack), symbol) if symbol.isDigit && stack.nonEmpty => tokens -> (stack + symbol.toString) // накапливаем цифры
      case ((tokens, stack), symbol) if symbol.isDigit && stack.isEmpty => tokens -> ("" + symbol.toString)

      // parse function names
      case ((tokens, stack), symbol) if symbol.isLetter && stack.nonEmpty => tokens -> (stack + symbol.toString)
      case ((tokens, stack), symbol) if symbol.isLetter && stack.isEmpty => tokens -> ("" + symbol.toString)

      // parse commas which is a function arguments separator
      case ((tokens, stack), ',') if stack.nonEmpty => ((tokens :+ numberOrFunction(stack)) :+ new Comma) -> ""
      case ((tokens, stack), ',') if stack.isEmpty => (tokens :+ new Comma) -> stack

      // parse comma which is used in floating numbers
      case ((tokens, stack), symbol@'.') => tokens -> (stack + symbol.toString)

      // parse operators
      case ((tokens, stack), symbol) if !symbol.isLetterOrDigit && stack.nonEmpty => ((tokens :+ numberOrFunction(stack)) :+ operator(symbol)) -> ""
      case ((tokens, stack), symbol) if !symbol.isLetterOrDigit && stack.isEmpty  => (tokens :+ operator(symbol)) -> ""

      // when we cannot determine what it is
      case (_, symbol) => throw new Exception(s"Unknown error on \"$symbol\" symbol")
    }

    /** At the end of tokenization pop number from stack if it's exist in the stack */
    expression.foldLeft(List.empty[Token] -> "")(parser) match {
      case (tokens, "")    => tokens
      case (tokens, stack) => tokens :+ Number(stack)
    }
  }

  /**
    * Transform infix notation to the RPN postfix notation using "Shunting-yard algorithm"
    * @param tokens is an input list of tokens in infix notation
    * @return a list of tokens in infix RPN notation
    */
  def rpn(tokens: List[Token]): List[Token] = {
    /** While we have an operator on the top of stack - compare priorities and move operator from stack to list */
    def whileOperator(op: Operator)(tokens: List[Token], stack: Stack[Token]): (List[Token], Stack[Token]) =
      if (stack.nonEmpty && (stack.top.isInstanceOf[Operator] || stack.top.isInstanceOf[Func]) && op <= stack.top) {
        stack.pop2 match { case (top, s) => whileOperator(op)(tokens :+ top, s) }
      } else {
        tokens -> (stack push op)
      }

    def whileNotOpenBracket(tokens: List[Token], stack: Stack[Token]): (List[Token], Stack[Token]) =
      if (stack.nonEmpty && stack.top.isInstanceOf[OpenBracket]) {
        tokens -> stack.pop
      } else if (stack.nonEmpty && !stack.top.isInstanceOf[OpenBracket]) {
        stack.pop2 match { case (top, s) => whileNotOpenBracket(tokens :+ top, s) }
      } else if (stack.nonEmpty && stack.top.isInstanceOf[Func]) {
        stack.pop2 match { case (top, s) => whileNotOpenBracket(tokens :+ top, s) }
      } else {
        tokens -> stack
      }

    tokens.foldLeft(List.empty[Token] -> Stack.empty[Token]) {
      /** when token is a number */
      case ((output, stack), number@Number(_)) => (output :+ number) -> stack
      /** when token is a function name */
      case ((output, stack), func: Func) => output -> (stack push func)
      /** when token is a comma */
      case ((output, stack), _: Comma) =>
        val popped = stack.takeWhile { // here we can check missing open bracket
          case _: OpenBracket => false
          case _              => true
        }
        output ++ popped -> stack.takeRight(stack.length - popped.length)
      /** when token is an operator */
      case ((output, stack), op: Operator) => whileOperator(op)(output, stack)
      /** when token is an open bracket */
      case ((output, stack), bracket: OpenBracket) => output -> (stack push bracket)
      /** when token is an closed bracket */
      case ((output, stack), _: CloseBracket) => whileNotOpenBracket(output, stack)
      /** in case of unknown token */
      case _ => throw new Exception("Unknown exception during rpn conversion")
    } match {
      case (output, stack) => output ++ stack
    }
  }

  def rpn(expression: String): List[Token] = rpn(tokenize(expression))

  /**
    * Evaluate RPN expression
    * @param tokens in RPN postfix notation
    * @return result of evaluation
    */
  def eval(tokens: List[Token]): BigDecimal = {
    def pop2execute(stack: Stack[BigDecimal])(func: (BigDecimal, BigDecimal) => BigDecimal): Stack[BigDecimal] = {
      val (second, s1) = stack.pop2
      val (first, s2) = s1.pop2
      func(first, second) -> s2 match {
        case (result, s) => s push result
      }
    }

    tokens.foldLeft(Stack.empty[BigDecimal]) {
      case (stack, Number(value)) => stack push BigDecimal(value)
      // basic operations
      case (stack, _: Plus)     => pop2execute(stack)(_ + _)
      case (stack, _: Minus)    => pop2execute(stack)(_ - _)
      case (stack, _: Multiply) => pop2execute(stack)(_ * _)
      case (stack, _: Divide)   => pop2execute(stack)(_ / _)
      case (stack, _: Power)    => pop2execute(stack)((first, second) => BigDecimal(pow(first.toDouble, second.toDouble)))
      // functions
      case (stack, _: Sin)      => stack.pop2 match { case (top, s) => s push sin(top.toDouble) }
      case (stack, _: Cos)      => stack.pop2 match { case (top, s) => s push cos(top.toDouble) }
      case (stack, _: Min)      => pop2execute(stack)((first, second) => BigDecimal(min(first.toDouble, second.toDouble)))
      case (stack, _: Max)      => pop2execute(stack)((first, second) => BigDecimal(max(first.toDouble, second.toDouble)))
      // in case of unknown operation or function
      case _ => throw new Exception("Cannot determine operation on evaluation step")
    } match {
      case stack => stack.top
    }
  }

  def eval(expression: String): BigDecimal = eval(rpn(expression))
}
