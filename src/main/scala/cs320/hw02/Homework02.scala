package cs320

import scala.util.parsing.combinator._

trait Homework02 extends Homework {
  // MUWAE abstract syntax trees
  trait MUWAE
  case class Num(nums: List[Int]) extends MUWAE                           // e ::= n
  case class Add(left: MUWAE, right: MUWAE) extends MUWAE                 //     | {+ e e}
  case class Sub(left: MUWAE, right: MUWAE) extends MUWAE                 //     | {- e e}
  case class With(name: String, expr: MUWAE, body: MUWAE) extends MUWAE   //     | {with {x e} e}
  case class Id(id: String) extends MUWAE                                 //     | x
  case class Min(left: MUWAE, mid: MUWAE, right: MUWAE) extends MUWAE     //     | {min e e e}
  case class Max(left: MUWAE, mid: MUWAE, right: MUWAE) extends MUWAE     //     | {max e e e}

  // Parser for MUWAE
  object MUWAE extends RegexParsers {
    def wrap[T](rule: Parser[T]): Parser[T] = "{" ~> rule <~ "}"
    lazy val int: Parser[Int] = """-?\d+""".r ^^ (_.toInt)
    lazy val str: Parser[String] = """[a-zA-Z][a-zA-Z0-9_-]*""".r
    lazy val expr: Parser[MUWAE] =
      int                                       ^^ { case n => Num(List(n)) }                  |
      wrap(rep(int))                            ^^ { case ns => Num(ns) }                      |
      wrap("+" ~> expr ~ expr)                  ^^ { case l ~ r => Add(l, r) }                 |
      wrap("-" ~> expr ~ expr)                  ^^ { case l ~ r => Sub(l, r) }                 |
      wrap("with" ~> wrap(str ~ expr) ~ expr)   ^^ { case x ~ i ~ b => With(x, i, b) }         |
      str                                       ^^ { case x => Id(x) }                         |
      wrap("min" ~> expr ~ expr ~ expr)   ^^ { case l ~ m ~ r => Min(l, m, r) }          |
      wrap("max" ~> expr ~ expr ~ expr)   ^^ { case l ~ m ~ r => Max(l, m, r) }
    def apply(str: String): MUWAE = parseAll(expr, str).getOrElse(error(s"bad syntax: $str"))
  }

  // Evaluate a MUWAE expression contained in a string
  def run(str: String): List[Int]
}
