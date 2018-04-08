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

  /**
    * Supported operators with specified priority
    */
  trait Operator extends Token
  case class Plus(value: String) extends Operator {
    override def priority: Int = 1
    override def toString: String = "+"
  }
  case class Minus(value: String) extends Operator {
    override def priority: Int = 1
    override def toString: String = "-"
  }
  case class Multiply(value: String) extends Operator {
    override def priority: Int = 2
  }
  case class Divide(value: String) extends Operator {
    override def priority: Int = 2
  }
  case class Power(value: String) extends Operator {
    override def priority: Int = 3
  }

  // другие управляющие символы
  case class OpenBracket(value: String) extends Token
  case class CloseBracket(value: String) extends Token
  case class Comma(value: String) extends Token

  // функции
  trait Func extends Token
  case class Sin(value: String) extends Func
  case class Min(value: String) extends Func
  case class Max(value: String) extends Func

  /**
    * Преобразование строки в список токенов, передаваемая пустая строка используется для накопления цифр в числах,
    * выступая по сути в виде стека (на базе строки) для символов а также для имен функций вроде cos(1), max(1, 2)
    * @param expression исходное выражение в виде строки
    * @return список с токенами
    */
  def tokenize(expression: String): List[Token] = {
    /** Определяем является строка обычным числом или названием функции */
    def numberOrFunction(s: String): Token = {
      /** Пытаемся найти поддерживаемые функции */
      def getFunction(funcName: String) = funcName match {
        case "sin" => Sin(funcName)
        case "min" => Min(funcName)
        case "max" => Max(funcName)
        case _     => throw new Exception(s"Unsupported function name $s")
      }

      s.forall(_.isDigit) match {
        case true => Number(s)
        case _    => getFunction(s)
      }
    }

    /** Получаем оператор на основании символа */
    def operator(symbol: Char): Token = symbol match {
      case '+' => Plus(symbol.toString)
      case '-' => Minus(symbol.toString)
      case '*' => Multiply(symbol.toString)
      case '/' => Divide(symbol.toString)
      case '^' => Power(symbol.toString)

      case '(' => OpenBracket(symbol.toString)
      case ')' => CloseBracket(symbol.toString)

      case _   => throw new Exception(s"Unsupported operator $symbol")
    }

    /** Парсер для чисел, имен функций и операторов */
    val parser: ((List[Token], String), Char) => (List[Token], String) = {
      case (meta, ' ') => meta // обработка пробелов
      // парсинг цифр
      case ((tokens, stack), symbol) if symbol.isDigit && stack.nonEmpty          => tokens -> (stack + symbol.toString) // накапливаем цифры
      case ((tokens, stack), symbol) if symbol.isDigit && stack.isEmpty           => tokens -> ("" + symbol.toString)

      // парсинг имен функций
      case ((tokens, stack), symbol) if symbol.isLetter && stack.nonEmpty         => tokens -> (stack + symbol.toString)
      case ((tokens, stack), symbol) if symbol.isLetter && stack.isEmpty          => tokens -> ("" + symbol.toString)

      // experimental
      case ((tokens, stack), ',') if stack.nonEmpty => ((tokens :+ numberOrFunction(stack)) :+ Comma(",")) -> ""
      case ((tokens, stack), ',') if stack.isEmpty => (tokens :+ Comma(",")) -> stack

      // парсинг операторов
      case ((tokens, stack), symbol) if !symbol.isLetterOrDigit && stack.nonEmpty => ((tokens :+ numberOrFunction(stack)) :+ operator(symbol)) -> ""
      case ((tokens, stack), symbol) if !symbol.isLetterOrDigit && stack.isEmpty  => (tokens :+ operator(symbol)) -> ""

      // когда условие не попало ни под одно условие
      case (_, symbol)                                                            => throw new Exception(s"Unknown error on $symbol")
    }

    // в конце процесса парсинга выкидываем число из стека если оно там осталось
    expression.foldLeft(List.empty[Token] -> "")(parser) match {
      case (tokens, "")    => tokens
      case (tokens, stack) => tokens :+ Number(stack)
    }
  }

  /**
    * Преобразование списка токенов в ОПН (постфиксная форма) используя "Shunting-yard algorithm"
    * @param tokens входящий список токенов
    * @return список токенов в ОПН
    */
  def rpn(tokens: List[Token]): List[Token] = {
    /**  Пока на вершине стека присутствует оператор - сравниваем приоритеты и перекладываем в список */
    def whileOperator(op: Operator)(tokens: List[Token], stack: Stack[Token]): (List[Token], Stack[Token]) =
      if (stack.nonEmpty && stack.top.isInstanceOf[Operator] && op <= stack.top) {
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

    val res = tokens.foldLeft(List.empty[Token] -> Stack.empty[Token]) {
      /** Если токен - число */
      case ((output, stack), number@Number(_)) =>
        (output :+ number) -> stack
      /** Если токен - название функции */
      case ((output, stack), func: Func)       =>
        output -> (stack push func)
      /** Если токен - запятая */
      case ((output, stack), Comma(_))         =>
        val popped = stack.takeWhile { // ? отказоустойчивость + чекнуть что не было открывающей скобки
          case OpenBracket(_) => false
          case _              => true
        }
        output ++ popped -> stack.takeRight(stack.length - popped.length)
      /** если токен - оператор */
      case ((output, stack), op: Operator) =>
        whileOperator(op)(output, stack)
      /** Если токен - открывающая скобка */
      case ((output, stack), bracket@OpenBracket(_)) =>
        output -> (stack push bracket)
      /** Если токен - закрывающая скобка */
      case ((output, stack), bracket@CloseBracket(_))
      => whileNotOpenBracket(output, stack)

      case _                                   =>
        throw new Exception("Unknown exception during rpn conversion")
    } /*match {
      case (output, stack) => output ++ stack
    }*/

    val output = res match {
      case (output, stack) => output ++ stack
    }

    output
  }

  def rpn(expression: String): List[Token] = rpn(tokenize(expression))

  /**
    * Evaluate RPN expression
    * @param tokens in RPN postfix notation
    * @return result of evaluation
    */
  def eval(tokens: List[Token]): Int = {
    def pop2execute(stack: Stack[Int])(func: (Int, Int) => Int): Stack[Int] = {
      val (second, s1) = stack.pop2
      val (first, s2) = s1.pop2
      (func(first, second) -> s2) match {
        case (result, stack) => stack push result
      }
    }

    tokens.foldLeft(Stack.empty[Int]) {
      case (stack, Number(value)) => stack push value.toInt

      case (stack, Plus(_))     => pop2execute(stack)(_ + _)
      case (stack, Minus(_))    => pop2execute(stack)(_ - _)
      case (stack, Multiply(_)) => pop2execute(stack)(_ * _)
      case (stack, Divide(_))   => pop2execute(stack)(_ / _)
      case (stack, Power(_))    => pop2execute(stack)((first, second) => pow(first, second).toInt)
      // functions
      case (stack, Min(_))      => pop2execute(stack)((first, second) => min(first, second))
      case (stack, Max(_))      => pop2execute(stack)((first, second) => max(first, second))

      case _ => throw new Exception("Cannot determine operation on evaluation step")
    } match {
      case stack => stack.top
    }
  }

  def eval(expression: String): Int = eval(rpn(expression))
}
