package cs320

import scala.util.parsing.combinator._

trait Homework07 extends Homework {
  // abstract syntax of KXCFAE
  trait KXCFAE
  case class Num(num: Int) extends KXCFAE                                   // e ::= n
  case class Add(left: KXCFAE, right: KXCFAE) extends KXCFAE                //     | {+ e e}
  case class Sub(left: KXCFAE, right: KXCFAE) extends KXCFAE                //     | {- e e}
  case class Id(name: String) extends KXCFAE                                //     | x
  case class Fun(params: List[String], body: KXCFAE) extends KXCFAE         //     | {fun {x} e}
  case class App(fun: KXCFAE, args: List[KXCFAE]) extends KXCFAE            //     | {e e}
  case class If0(cond: KXCFAE, thenE: KXCFAE, elseE: KXCFAE) extends KXCFAE //     | {if0 e e e}
  case class Withcc(name: String, body: KXCFAE) extends KXCFAE              //     | {withcc x e}
  case class Try(tryE: KXCFAE, catchE: KXCFAE) extends KXCFAE               //     | {try e catch e}
  case object Throw extends KXCFAE                                          //     | {throw}

  // Parser for KXCFAE
  object KXCFAE extends RegexParsers {
    def wrap[T](rule: Parser[T]): Parser[T] = "{" ~> rule <~ "}"
    lazy val int: Parser[Int] = """-?\d+""".r ^^ (_.toInt)
    lazy val str: Parser[String] = """[a-zA-Z][a-zA-Z0-9_-]*""".r
    lazy val expr: Parser[KXCFAE] =
      int                                       ^^ { case n => Num(n) }                        |
      wrap("+" ~> expr ~ expr)                  ^^ { case l ~ r => Add(l, r) }                 |
      wrap("-" ~> expr ~ expr)                  ^^ { case l ~ r => Sub(l, r) }                 |
      str                                       ^^ { case x => Id(x) }                         |
      wrap("fun" ~> wrap(rep(str)) ~ expr)      ^^ { case ps ~ b => Fun(ps, b) }               |
      wrap("if0" ~> expr ~ expr ~ expr)         ^^ { case c ~ t ~ e => If0(c, t, e) }          |
      wrap("withcc" ~> str ~ expr)              ^^ { case x ~ b => Withcc(x, b) }              |
      wrap("try" ~> expr ~ "catch" ~ expr)      ^^ { case t ~ _ ~ c => Try(t, c) }             |
      wrap("throw")                             ^^ { case _ => Throw }                         |
      wrap(expr ~ rep(expr))                    ^^ { case f ~ as => App(f, as) }
    def apply(str: String): KXCFAE = parseAll(expr, str).getOrElse(error(s"bad syntax: $str"))
  }

  // Evaluate a KXCFAE program contained in a string
  def run(str: String): String
}
