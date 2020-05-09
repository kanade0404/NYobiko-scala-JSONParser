package jp.ed.nnn.parsercombinator

class ArithParser extends Combinator {
  def expr: Parser[Any] = term ~ rep(s("+") ~ term | s("-") ~ term)
  def term: Parser[Any] = factor ~ rep(s("*") ~ factor | s("/") ~ factor)
  def factor: Parser[Any] = floatingPointNumber | s("(") ~ factor ~ s(")")
  def apply(input: String): Any = expr(input)
}
