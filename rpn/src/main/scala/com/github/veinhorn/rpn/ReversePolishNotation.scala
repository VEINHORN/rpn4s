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
  class Dot extends Token

  // функции
  trait Func extends Token {
    override def priority: Int = 4
  }
  class Sin extends Func
  class Cos extends Func
  class Min extends Func
  class Max extends Func

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

      // парсинг запятой в аргументах функции
      case ((tokens, stack), ',') if stack.nonEmpty => ((tokens :+ numberOrFunction(stack)) :+ Comma(",")) -> ""
      case ((tokens, stack), ',') if stack.isEmpty => (tokens :+ Comma(",")) -> stack

      // парсинг точки (experimental)
      case ((tokens, stack), symbol@'.') => tokens -> (stack + symbol.toString)

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
      /** Если токен - число */
      case ((output, stack), number@Number(_)) => (output :+ number) -> stack
      /** Если токен - название функции */
      case ((output, stack), func: Func) => output -> (stack push func)
      /** Если токен - запятая */
      case ((output, stack), Comma(_)) =>
        val popped = stack.takeWhile { // можно еще как-то проверить что не было открывающей скобки
          case OpenBracket(_) => false
          case _              => true
        }
        output ++ popped -> stack.takeRight(stack.length - popped.length)
      /** если токен - оператор */
      case ((output, stack), op: Operator) => whileOperator(op)(output, stack)
      /** Если токен - открывающая скобка */
      case ((output, stack), bracket@OpenBracket(_)) => output -> (stack push bracket)
      /** Если токен - закрывающая скобка */
      case ((output, stack), bracket@CloseBracket(_)) => whileNotOpenBracket(output, stack)

      case _                                   =>
        throw new Exception("Unknown exception during rpn conversion")
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

      case (stack, Plus(_))     => pop2execute(stack)(_ + _)
      case (stack, Minus(_))    => pop2execute(stack)(_ - _)
      case (stack, Multiply(_)) => pop2execute(stack)(_ * _)
      case (stack, Divide(_))   => pop2execute(stack)(_ / _)
      case (stack, Power(_))    => pop2execute(stack)((first, second) => BigDecimal(pow(first.toDouble, second.toDouble)))
      // functions
      case (stack, _: Sin)      => stack.pop2 match { case (top, s) => s push sin(top.toDouble) }
      case (stack, _: Cos)      => stack.pop2 match { case (top, s) => s push cos(top.toDouble) }
      case (stack, _: Min)      => pop2execute(stack)((first, second) => BigDecimal(min(first.toDouble, second.toDouble)))
      case (stack, _: Max)      => pop2execute(stack)((first, second) => BigDecimal(max(first.toDouble, second.toDouble)))

      case _ => throw new Exception("Cannot determine operation on evaluation step")
    } match {
      case stack => stack.top
    }
  }

  def eval(expression: String): BigDecimal = eval(rpn(expression))
}
