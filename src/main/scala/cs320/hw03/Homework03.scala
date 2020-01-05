package cs320

import scala.util.parsing.combinator._

trait Homework03 extends Homework {
  // MRFWAE type
  trait MRFWAE
  case class Num(num: Int) extends MRFWAE                                   // e ::= n
  case class Add(left: MRFWAE, right: MRFWAE) extends MRFWAE                //     | {+ e e}
  case class Sub(left: MRFWAE, right: MRFWAE) extends MRFWAE                //     | {- e e}
  case class With(name: String, value: MRFWAE, body: MRFWAE) extends MRFWAE //     | {with {x e} e}
  case class Id(name: String) extends MRFWAE                                //     | x
  case class App(func: MRFWAE, args: List[MRFWAE]) extends MRFWAE           //     | {e e*}
  case class Fun(params: List[String], body: MRFWAE) extends MRFWAE         //     | {fun {x*} e}
  case class Rec(rec: Map[String, MRFWAE]) extends MRFWAE                   //     | {record {x e}*}
  case class Acc(expr: MRFWAE, name: String) extends MRFWAE                 //     | {access e x}

  // Check duplicated string values in a given string list.
  def dupCheck(ss: List[String]): Boolean = ss match {
    case h :: t => (t contains h) || dupCheck(t)
    case Nil => false
  }

  // Parser for MRFWAE
  object MRFWAE extends RegexParsers {
    def wrap[T](rule: Parser[T]): Parser[T] = "{" ~> rule <~ "}"
    lazy val int: Parser[Int] = """-?\d+""".r ^^ (_.toInt)
    lazy val str: Parser[String] = """[a-zA-Z][a-zA-Z0-9_-]*""".r
    lazy val expr: Parser[MRFWAE] =
      int                                       ^^ { case n => Num(n) }                        |
      wrap("+" ~> expr ~ expr)                  ^^ { case l ~ r => Add(l, r) }                 |
      wrap("-" ~> expr ~ expr)                  ^^ { case l ~ r => Sub(l, r) }                 |
      wrap("with" ~> wrap(str ~ expr) ~ expr)   ^^ { case x ~ i ~ b => With(x, i, b) }         |
      str                                       ^^ { case x => Id(x) }                         |
      wrap("fun" ~> wrap(rep(str)) ~ expr)      ^^ { case ps ~ b =>
        if (dupCheck(ps)) error(s"bad syntax: duplicate parameters: $ps")
        else Fun(ps, b)
      } |
      wrap("record" ~> rep(wrap(str ~ expr)))   ^^ { case ps =>
        val pList = ps.map { case f ~ e => (f,e) }
        if (dupCheck(pList.map { case (f, e) => f })) error(s"duplicate fields: $pList")
        Rec(pList.toMap)
      } |
      wrap("access" ~> expr ~ str)              ^^ { case e ~ x => Acc(e, x) }                 |
      wrap(expr ~ rep(expr))                    ^^ { case f ~ as => App(f, as) }
    def apply(str: String): MRFWAE = parseAll(expr, str).getOrElse(error(s"bad syntax: $str"))
  }

  // Evaluate a MRFWAE program contained in a string
  def run(str: String): String
}
