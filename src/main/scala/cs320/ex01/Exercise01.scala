package cs320

import scala.util.parsing.combinator._

trait Exercise01 extends Homework {
  // WAE abstract syntax trees
  trait WAE
  case class Num(num: Int) extends WAE                                 // e ::= n
  case class Add(left: WAE, right: WAE) extends WAE                    //     | {+ e e}
  case class Sub(left: WAE, right: WAE) extends WAE                    //     | {- e e}
  case class With(name: String, expr: WAE, body: WAE) extends WAE      //     | {with {x e} e}
  case class Id(id: String) extends WAE                                //     | x

  // Parser for WAE
  object WAE extends RegexParsers {
    def wrap[T](rule: Parser[T]): Parser[T] = "{" ~> rule <~ "}"
    lazy val int: Parser[Int] = """-?\d+""".r ^^ (_.toInt)
    lazy val str: Parser[String] = """[a-zA-Z0-9]+""".r
    lazy val expr: Parser[WAE] =
      int                                       ^^ { case n => Num(n) }                        |
      wrap("+" ~> expr ~ expr)                  ^^ { case l ~ r => Add(l, r) }                 |
      wrap("-" ~> expr ~ expr)                  ^^ { case l ~ r => Sub(l, r) }                 |
      wrap("with" ~> wrap(str ~ expr) ~ expr)   ^^ { case x ~ i ~ b => With(x, i, b) }         |
      str                                       ^^ { case x => Id(x) }
    def apply(str: String): WAE = parseAll(expr, str).getOrElse(error(s"bad syntax: $str"))
  }

  // Problem 1
  def freeIds(expr: WAE): Set[String]

  // Problem 2
  def bindingIds(expr: WAE): Set[String]

  // Problem 3
  def boundIds(expr: WAE): Set[String]

  // Tests
  def tests: Unit
}
