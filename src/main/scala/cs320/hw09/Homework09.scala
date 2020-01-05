package cs320

import scala.util.parsing.combinator._

trait Homework09 extends Homework with RegexParsers with PackratParsers {
  // Mini Scala Program
  case class Program(stmts: List[Stmt], expr: Expr)

  object Program extends ParserObject(prog)

  // Expressions
  trait Expr
  case class Num(num: Int) extends Expr
  case class Bool(bool: Boolean) extends Expr
  case class Add(left: Expr, right: Expr) extends Expr
  case class Sub(left: Expr, right: Expr) extends Expr
  case class Eq(left: Expr, right: Expr) extends Expr
  case class Lt(left: Expr, right: Expr) extends Expr
  case class Id(name: String) extends Expr
  case class Fun(params: List[(String, Type)], body: Expr) extends Expr
  case class App(func: Expr, args: List[Expr]) extends Expr
  case class Block(stmts: List[Stmt], expr: Expr) extends Expr
  case class Assign(name: String, expr: Expr) extends Expr
  case class Match(expr: Expr, cases: Map[String, (List[String], Expr)]) extends Expr
  case class IfThenElse(cond: Expr, thenE: Expr, elseE: Expr) extends Expr

  object Expr extends ParserObject(expr)

  // statements
  trait Stmt
  case class Val(isLazy: Boolean, name: String, ty: Type, expr: Expr) extends Stmt
  case class Var(name: String, ty: Type, expr: Expr) extends Stmt
  case class Def(name: String, params: List[(String, Type)], retTy: Type, body: Expr) extends Stmt
  case class Trait(name: String, cases: Map[String, List[Type]]) extends Stmt

  object Stmt extends ParserObject(stmt)

  // Type
  trait Type
  case object NumT extends Type
  case object BoolT extends Type
  case class ArrowT(ps: List[Type], r: Type) extends Type
  case class IdT(x: String) extends Type

  object Type extends ParserObject(ty)

  // Value
  trait Value {
    override def toString: String = this match {
      case NumV(n) => n.toString
      case BoolV(b) => b.toString
      case CloV(_, _, _) => "<function>"
      case VariantV(x, _) => s"<variant:$x>"
      case ConstructorV(x) => s"<constructor:$x>"
      case ExprV(_, _) => s"<lazy>"
      case AddrV(_) => s"<addr>"
    }
  }
  case class NumV(num: Int) extends Value
  case class BoolV(bool: Boolean) extends Value
  case class CloV(params: List[String], body: Expr, var env: Env) extends Value
  case class VariantV(name: String, values: List[Value]) extends Value
  case class ConstructorV(name: String) extends Value
  case class ExprV(expr: Expr, env: Env) extends Value
  case class AddrV(addr: Addr) extends Value

  // Environments and Stores
  type Addr = Int
  type Env = Map[String, Value]
  type Sto = Map[Addr, Value]

  // Type Environments
  case class TyEnv(
    mutables: Set[String] = Set(),
    varMap: Map[String, Type] = Map(),
    tbinds: Map[String, Map[String, List[Type]]] = Map()
  ) {
    def addVar(x: String, ty: Type, mutable: Boolean): TyEnv =
      if (mutable) copy(varMap = varMap + (x -> ty), mutables = mutables + x)
      else copy(varMap = varMap + (x -> ty))
    def addTBind(x: String, map: Map[String, List[Type]]): TyEnv =
      copy(tbinds = tbinds + (x -> map))
  }

  // Interpreter with Type Checker
  def run(str: String): String = {
    val p: Program = Program(str)
    typeCheck(p)
    interp(p).toString
  }

  // Type Checker
  def typeCheck(expr: Expr, tyEnv: TyEnv): Type
  def typeCheck(tyEnv: TyEnv, stmt: Stmt): TyEnv
  def typeCheck(pr: Program): Type =
    typeCheck(pr.expr, (TyEnv() /: pr.stmts)(typeCheck))

  // Interpreter
  def interp(expr: Expr, env: Env, sto: Sto): (Value, Sto)
  def interp(pair: (Env, Sto), stmt: Stmt): (Env, Sto)
  def interp(pr: Program): Value = {
    val (env, sto) = ((Map[String, Value](), Map[Addr, Value]()) /: pr.stmts)(interp)
    val (v, _) = interp(pr.expr, env, sto)
    v
  }

  // Parsers
  abstract class ParserObject[T](parser: PackratParser[T]) {
    def apply(str: String): T = parseAll(parser, str).getOrElse(error(s"bad syntax: $str"))
  }
  def wrap[T](rule: PackratParser[T]): PackratParser[T] = "{" ~> rule <~ "}"
  lazy val int: PackratParser[Int] = """-?\d+""".r ^^ (_.toInt)
  lazy val str: PackratParser[String] = regex("""[a-zA-Z][a-zA-Z0-9_-]*""".r)
  lazy val prog: PackratParser[Program] =
    rep(stmt <~ opt(";")) ~ expr ^^ { case is ~ e => Program(is, e) }
  lazy val stmt: PackratParser[Stmt] = (
    (opt("lazy") <~ "val") ~ str ~ (":" ~> ty <~ "=") ~ expr ^^ {
      case l ~ x ~ t ~ e => Val(l.isDefined, x, t, e)
    } | "var" ~> str ~ (":" ~> ty <~ "=") ~ expr ^^ {
      case x ~ t ~ e => Var(x, t, e)
    } | "def" ~> str ~ ("(" ~> repsep(str ~ (":" ~> ty), ",") <~ ")" ~ ":") ~ (ty <~ "=") ~ expr ^^ {
      case x ~ pts ~ r ~ e => Def(x, pts.map { case p ~ t => (p, t) }, r, e)
    } | "trait" ~> str ~ rep1("case" ~ "class" ~> str ~ ("(" ~> repsep(ty, ",") <~ ")")) ^^ {
      case x ~ cs => Trait(x, cs.map { case x ~ ts => x -> ts }.toMap)
    }
  )
  lazy val expr: PackratParser[Expr] = (
    int ^^ {
      Num(_)
    } | ("true" | "false") ^^ {
      case x => Bool(x == "true")
    } | (expr <~ "+") ~ expr ^^ {
      case x ~ y => Add(x, y)
    } | (expr <~ "-") ~ expr ^^ {
      case x ~ y => Sub(x, y)
    } | (expr <~ "==") ~ expr ^^ {
      case x ~ y => Eq(x, y)
    } | (expr <~ "<") ~ expr ^^ {
      case x ~ y => Lt(x, y)
    } | "(" ~> expr <~ ")" ^^ {
      case x => x
    } | ("(" ~> repsep(str ~ (":" ~> ty), ",") <~ ")" ~ "=>") ~ expr ^^ {
      case pts ~ e => Fun(pts.map { case p ~ t => (p, t) }, e)
    } | expr ~ ("(" ~> repsep(expr, ",") <~ ")") ^^ {
      case f ~ as => App(f, as)
    } | "{" ~> rep(stmt <~ opt(";")) ~ expr <~ "}" ^^ {
      case is ~ e => Block(is, e)
    } | (str <~ "=") ~ expr ^^ {
      case x ~ e => Assign(x, e)
    } | expr ~ ("match" ~ "{" ~> rep1("case" ~> str ~ ("(" ~> repsep(str, ",") <~ ")" ~ "=>") ~ expr) <~ "}") ^^ {
      case e ~ cs => Match(e, cs.map { case x ~ xs ~ e => x -> (xs, e) }.toMap)
    } | ("if" ~ "(" ~> expr <~ ")") ~ expr ~ ("else" ~> expr) ^^ {
      case c ~ t ~ e => IfThenElse(c, t, e)
    } | str ^^ { Id(_) }
  )
  lazy val ty: PackratParser[Type] = (
    str ^^ {
      case "Int" => NumT
      case "Boolean" => BoolT
      case x => IdT(x)
    }
  )
}
