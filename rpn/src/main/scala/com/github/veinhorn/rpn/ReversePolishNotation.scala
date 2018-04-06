package com.github.veinhorn.rpn

import scala.collection.immutable.Stack

/**
  * Created by VEINHORN on 06.04.2018.
  */
object ReversePolishNotation {

  trait Token

  case class Number(value: String) extends Token

  // операторы
  case class Plus(value: String) extends Token
  case class Minus(value: String) extends Token
  case class Multiply(value: String) extends Token
  case class Divide(value: String) extends Token
  case class OpenBracket(value: String) extends Token
  case class CloseBracket(value: String) extends Token

  // функции
  trait Func extends Token
  case class Sin(value: String) extends Func

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
      // парсинг операторов
      case ((tokens, stack), symbol) if !symbol.isLetterOrDigit && stack.nonEmpty => ((tokens :+ numberOrFunction(stack)) :+ operator(symbol)) -> ""
      case ((tokens, stack), symbol) if !symbol.isLetterOrDigit && stack.isEmpty  => (tokens :+ operator(symbol)) -> ""
      // парсинг имен функций
      case ((tokens, stack), symbol) if symbol.isLetter && stack.nonEmpty         => tokens -> (stack + symbol.toString)
      case ((tokens, stack), symbol) if symbol.isLetter && stack.isEmpty          => tokens -> ("" + symbol.toString)
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


    tokens.foldLeft(List.empty[Token] -> Stack.empty[Token]) {
      /** Если токен - число */
      case ((output, stack), number@Number(_)) => (output :+ number) -> stack
      /** Если токен - название функции */
      case ((output, stack), func: Func)       => output -> (stack push func)

      case _                                   => throw new Exception("Unknown exception during rpn conversion")
    } match {
      case (output, stack) => output ++ stack
    }
  }

  def rpn(expression: String): List[Token] = rpn(tokenize(expression))

}
