package jp.ed.nnn.parsercombinator

object ParserStudy {
  sealed trait ParseResult[+T]
  case class Success[+T](value: T, next: String) extends ParseResult[T]
  case object Failure extends ParseResult[Nothing]
  type Parser[+T] = String => ParseResult[T]
  def trueParser: Parser[Boolean] = input =>
    if (input.startsWith("true")) {
      Success(true, input.substring("true".length))
    } else {
      Failure
    }
  def falseParser: Parser[Boolean] = input =>
    if (input.startsWith("false")) {
      Success(false, input.substring("false".length))
    } else {
      Failure
    }

  /**
    * 結果がSuccessならas句で取得したSuccess型の結果を返す。
    * パースに失敗した場合はfalseParserでの結果を返す。
    * @return パースした結果を返す
    */
  def booleanParser: Parser[Boolean] = input =>
    trueParser(input) match {
      case success@Success(_, _) => success
      case Failure => falseParser(input)
    }
}
