package cs320

package object hw06 extends Homework06 {  
  trait SRBFAEValue
  case class NumV(n: Int) extends SRBFAEValue
  case class CloV(param: String,
                  body: SRBFAE,
                  env: Env) extends SRBFAEValue
  case class BoxV(addr: Addr) extends SRBFAEValue
  case class RecV(rec: Env) extends SRBFAEValue

  type Addr = Int
  type Sto = Map[Addr, SRBFAEValue]
  type Env = Map[String, Addr]

  def storeLookup(addr: Addr, sto: Sto): SRBFAEValue  = sto.getOrElse(addr, error("no such field"))
  def lookup(name: String, env: Env): Addr = env.getOrElse(name, error(s"free identifier: $name"))
  def lookupRec(name: String, env: Env): Addr = env.getOrElse(name, error("no such field"))


  def numOpV(op: (Int, Int) => Int): (SRBFAEValue, SRBFAEValue) => SRBFAEValue = (_, _) match {
    case (NumV(x), NumV(y)) => NumV(op(x, y))
    case (x, y) => error(s"not both numbers: $x, $y")
  }
  val numAddV = numOpV(_ + _)
  val numSubV = numOpV(_ - _)

  def malloc(sto: Sto): Addr =  
      sto.foldLeft(0) {
        case (max, (addr, _)) => math.max(max, addr)
      } + 1

  def interpRecV(fields: List[(String, SRBFAE)], env: Env, sto: Sto, subEnv: Env): (Env, Sto) = {
    fields match {
        case Nil => (subEnv, sto)
        case (name, expr) :: tl => 
            val (v, s) = interp(expr, env, sto)
            val addr = malloc(s)
            interpRecV(tl, env + (name -> addr), s + (addr -> v), subEnv + (name -> addr))
    }
  }

  def interp(srbfae: SRBFAE, env: Env, sto: Sto): (SRBFAEValue, Sto) = 
      srbfae match  {
          case Num(num) => (NumV(num), sto)

          case Add(left, right) => 
              val (lv, ls) = interp(left, env, sto)
              val (rv, rs) = interp(right, env, ls)
              (numAddV(lv, rv), rs)

          case Sub(left, right) => 
              val (lv, ls) = interp(left, env, sto)
              val (rv, rs) = interp(right, env, ls)
              (numSubV(lv, rv), rs)

          case Id(name) => (storeLookup(lookup(name, env), sto), sto)
          case Fun(param, body) => (CloV(param, body, env), sto)

          case App(fun, arg) => arg match {
              case Id(name) =>
                  val (fv, fs) = interp(fun, env, sto)
                  fv match {
                      case CloV(param, body, fenv) =>
                          val addr = lookup(name, env)
                          interp(body, fenv + (param -> addr), fs)
                      case _ => error(s"not a closure: $fv")
                  }
              case _ => 
                  val (fv, fs) = interp(fun, env, sto)
                  val (av, as) = interp(arg, env, fs)
                  fv match {
                      case CloV(param, body, fenv) =>
                          val addr = malloc(as)
                          interp(body, fenv + (param -> addr), as + (addr -> av))
                      case _ => error(s"not a closure: $fv")
                  }
              }

          case NewBox(expr) => 
              val (v, s) = interp(expr, env, sto)
              val addr = malloc(s)
              (BoxV(addr), s + (addr -> v))
          case OpenBox(box) => 
              val (bv, bs) = interp(box, env, sto)
              bv match {
                  case BoxV(addr) =>(storeLookup(addr, bs), bs)
                  case _ => error(s"not a box: $bv")
              }
          case SetBox(box, expr) => 
              val (bv, bs) = interp(box, env, sto)
              bv match {
                  case BoxV(addr) =>
                      val (v, s) = interp(expr, env, bs)
                      (v, s + (addr -> v))
                  case _ => error(s"not a box: $bv")
              }

          case Seqn(left, right) => 
              val (lv, ls) = interp(left, env, sto)
              right match {
                case Nil => (lv, ls)
                case head :: tl =>  interp(Seqn(head, tl), env, ls)
              }
          
          case Rec(fields) => interpRecV(fields, env, sto, Map()) match {
              case (rec, s) => (RecV(rec), s)
            }
          case Get(record, field) => interp(record, env, sto) match {
            case (RecV(rec), s) => 
                (storeLookup(lookupRec(field, rec), s), s)
            case _ => error("not record")
          }
              
          case Set(record, field, expr) => interp(record, env, sto) match {
            case (RecV(rec), st) =>
                val (v, s) = interp(expr, env, st)
                (v, s + (lookupRec(field, rec) -> v))
            case _ => error("not record")
          }
      }

  def run(str: String): String = interp(SRBFAE(str), Map(), Map()) match {
      case (NumV(num),sto) => num.toString()
      case (CloV(param, body, env), sto) => "function"
      case (BoxV(addr), sto) => "box"
      case (RecV(rec), sto) => "record"
  }

  def tests: Unit = {
    test(run("""{{fun {b} {seqn {setbox b {+ 2 {openbox b}}}
                          {setbox b {+ 3 {openbox b}}}
                          {setbox b {+ 4 {openbox b}}}
                          {openbox b}}}
                {newbox 1}}"""), "10")
    testExc(run("{get {rec {x 1}} y}"), "no such field")
    test(run("{{fun {r} {seqn {set r x 5} {get r x}}} {rec {y 2} {x 1}}}"), "5")
    test(run("42"), "42")
    test(run("{fun {x} x}"), "function")
    test(run("{newbox 1}"), "box")
    test(run("{rec}"), "record")

    /* Write your own tests */
    test(run("""{seqn 1 2 3 4 5 6 7 8 9 10}"""), "10")
    testExc(run("""{get {rec {a {fun {x} x}}} b}"""), "no such field")
    test(run("""{openbox {newbox 2000}}"""), "2000")
    test(run("""{newbox {fun {x} x}}"""), "box")
    testExc(run("""{get {fun {x} x} x}"""), "")
    test(run("{rec {a {get {rec {b 0}} b}} {c 2}}"), "record")
    test(run("""{+ 31 69}"""), "100")
    test(run("{fun {x} {+ x y}}"), "function")
    test(run("{{fun {x} {get {rec {y 2000}} y}} {seqn 1 2 3 4 5 6 7 8 9 10}}"), "2000")


  }
}
