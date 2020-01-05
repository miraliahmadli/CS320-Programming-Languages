package cs320

import scala.util.parsing.combinator._

trait Homework06 extends Homework {
  // SRBFAE type
  trait SRBFAE
  case class Num(num: Int) extends SRBFAE                                     // e ::= n
  case class Add(left: SRBFAE, right: SRBFAE) extends SRBFAE                  //     | {+ e e}
  case class Sub(left: SRBFAE, right: SRBFAE) extends SRBFAE                  //     | {- e e}
  case class Id(name: String) extends SRBFAE                                  //     | x
  case class Fun(param: String, body: SRBFAE) extends SRBFAE                  //     | {fun {x} e}
  case class App(fun: SRBFAE, arg: SRBFAE) extends SRBFAE                     //     | {e e}
  case class NewBox(expr: SRBFAE) extends SRBFAE                              //     | {newbox e}
  case class SetBox(box: SRBFAE, expr: SRBFAE) extends SRBFAE                 //     | {setbox e e}
  case class OpenBox(box: SRBFAE) extends SRBFAE                              //     | {openbox e}
  case class Seqn(left: SRBFAE, right: List[SRBFAE]) extends SRBFAE           //     | {seqn e e*}
  case class Rec(fields: List[(String, SRBFAE)]) extends SRBFAE               //     | {rec {<id> e}*}
  case class Get(record: SRBFAE, field: String) extends SRBFAE                //     | {get e <id>}
  case class Set(record: SRBFAE, field: String, expr: SRBFAE) extends SRBFAE  //     | {set e x e}

  // Parser for SRBFAE
  object SRBFAE extends RegexParsers {
    def wrap[T](rule: Parser[T]): Parser[T] = "{" ~> rule <~ "}"
    lazy val int: Parser[Int] = """-?\d+""".r ^^ (_.toInt)
    lazy val str: Parser[String] = """[a-zA-Z][a-zA-Z0-9_-]*""".r
    lazy val expr: Parser[SRBFAE] =
      int                                       ^^ { case n => Num(n) }                        |
      wrap("+" ~> expr ~ expr)                  ^^ { case l ~ r => Add(l, r) }                 |
      wrap("-" ~> expr ~ expr)                  ^^ { case l ~ r => Sub(l, r) }                 |
      str                                       ^^ { case x => Id(x) }                         |
      wrap("fun" ~> wrap(str) ~ expr)           ^^ { case p ~ b => Fun(p, b) }                 |
      wrap("newbox" ~> expr)                    ^^ { case e => NewBox(e) }                     |
      wrap("setbox" ~> expr ~ expr)             ^^ { case b ~ e => SetBox(b, e) }              |
      wrap("openbox" ~> expr)                   ^^ { case e => OpenBox(e) }                    |
      wrap("seqn" ~> expr ~ rep(expr))          ^^ { case l ~ rs => Seqn(l, rs) }              |
      wrap("rec" ~> rep(wrap(str ~ expr)))      ^^ { case f =>
        Rec(f.map({ case f ~ e => f -> e }).toList)
      }                                                                                        |
      wrap("get" ~> expr ~ str)                 ^^ { case r ~ f => Get(r, f) }                 |
      wrap("set" ~> expr ~ str ~ expr)          ^^ { case r ~ f ~ e => Set(r, f, e) }          |
      wrap(expr ~ expr)                         ^^ { case f ~ a => App(f, a) }
    def apply(str: String): SRBFAE = parseAll(expr, str).getOrElse(error(s"bad syntax: $str"))
  }

  // Evaluate a SRBFAE program contained in a string
  def run(str: String): String
}
