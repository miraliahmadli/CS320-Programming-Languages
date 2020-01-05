package cs320

package object hw07 extends Homework07 {

  trait KXCFAEValue
  case class NumV(n: Int) extends KXCFAEValue
  case class CloV(params: List[String],
                  body: KXCFAE,
                  env: Env) extends KXCFAEValue
  case class ContV(proc: Cont) extends KXCFAEValue
  case object ThrowV extends KXCFAEValue
  
  type Env = Map[String, KXCFAEValue]
  
  type Cont = KXCFAEValue => KXCFAEValue

  def numOpV(op: (Int, Int) => Int): (KXCFAEValue, KXCFAEValue) => KXCFAEValue = (_, _) match {
    case (NumV(x), NumV(y)) => NumV(op(x, y))
    case (ThrowV, _) => ThrowV
    case (_, ThrowV) => ThrowV 
    case (x, y) => error(s"not both numbers: $x, $y")
  }
  val numAddV = numOpV(_ + _)
  val numSubV = numOpV(_ - _)

  def interpApp(params: List[String], args: List[KXCFAE], body: KXCFAE, env: Env, fenv: Env, k:Cont): KXCFAEValue =
      if (args.length != params.length) error("wrong arity")
      else args match{
        case Nil => interp(body, fenv, k)
        case arg :: rest => 
          interp(arg, env, argv => 
            argv match{
              case ThrowV => k(ThrowV)
              case _ => params match{
                case param :: tail => interpApp(tail, rest, body, env,fenv + (param -> argv), k)
              }   
            }
          )
      }

  def interp(kxcfae: KXCFAE, env: Env, k: Cont) : KXCFAEValue = 
      kxcfae match {
        case Num(num) => k(NumV(num))
        case Id(name) => k(env.getOrElse(name, error(s"free identifier: $name")))
        case Add(left, right) => 
            interp(left, env, lv => lv match{
              case ThrowV => k(ThrowV)
              case  _ =>
                  interp(right, env, rv =>
                      k(numAddV(lv, rv)))
            })
        case Sub(left, right) => 
            interp(left, env, lv => lv match{
              case ThrowV => k(ThrowV)
              case  _ =>
                  interp(right, env, rv =>
                      k(numSubV(lv, rv)))
            })
        case Fun(params, body) => k(CloV(params, body, env))
        case App(fun, args) => 
          interp(fun, env, fv =>
            fv match {
                case ThrowV => k(ThrowV)
                case ContV(proc) => args match {
                  case Nil => ContV(proc)
                  case head :: tl => interp(head, env, argv =>  proc(argv))
                }
                case CloV(params, body, fenv) => interpApp(params, args, body, env, fenv, k)
                case v =>error("not a closure: $v")
              }
            )
        case If0(cond, thenE, elseE) => 
            interp(cond, env, condv =>
                condv match{
                  case ThrowV => k(ThrowV)
                  case NumV(0) => interp(thenE, env, thenv => k(thenv))
                  case _ => interp(elseE, env, elsev => k(elsev))
                }
            )
        case Withcc(name, body) => interp(body, env + (name -> ContV(k)), k)
        case Try(tryE, catchE) => interp(tryE, env, tryv => tryv match{
            case ThrowV => interp(catchE, env, k)
            case _ => k(tryv)
        })
        case Throw => k(ThrowV)
      }

  def run(str: String): String = interp(KXCFAE(str), Map(), x=>x) match {
    case NumV(n) => n.toString()
    case CloV(params, body, env) => "function" 
    case ContV(k) => "continuation"
    case ThrowV => error("no enclosing try-catch")
  }

def tests: Unit = {
    test(run("{{fun {x y} {- y x}} 10 12}"), "2")
    test(run("{fun {} 12}"), "function")
    test(run("{withcc esc {{fun {x y} x} 1 {esc 3}}}"), "3")
    test(run("{try 7 catch 8}"), "7")
    test(run("{try {throw} catch 8}"), "8")
    test(run("{try {+ 1 {throw}} catch 8}"), "8")
    test(run("{{fun {f} {try {f 3} catch 8}} {fun {x} {throw}}}"), "8")
    test(run("{try {try {throw} catch {throw}} catch 9}"), "9")
    test(run("{try {try 7 catch {throw}} catch 9}"), "7")
    test(run("{{withcc esc {try {{withcc k {esc k}} 0} catch {fun {x} 8}}} {fun {x} {throw}}}"), "8")
    test(run("{fun {x} {fun {} x}}"), "function")
    test(run("{{{fun {x} {fun {} x}} 13}}"), "13")
    test(run("{+ {withcc k {k 5}} 4}"), "9")
    test(run("{withcc k {- 0 {k 100}}}"), "100")
    test(run("{withcc k {k {+ 100 11}}}"), "111")
    test(run("{{fun {a b c} {- {+ {withcc k {+ {k 100} a}} b} c}} 100 200 300}"), "0")
    test(run("{{withcc esc {{fun {x y} {fun {z} {+ z y}}} 1 {withcc k {esc k}}}} 10}"), "20")
    test(run("{withcc done {{fun {f x} {f f x}} {fun {g y} {if0 {- y 1} {throw} {try {+ y {g g {- y 1}}} catch {done y}}}} 10}}"), "2")
    test(run("{try {{fun {x y z} {a b c}} 1 2 {throw}} catch 0}"), "0")
    test(run("{try {- 0 {withcc k {+ 3 {k {throw}}}}} catch 89}"), "89")
    test(run("{try {+ 3 {withcc k {+ 1000 {k {throw}}}}} catch 11}"), "11")
    test(run("{{fun {x y z} {try {+ 1 {+ x {throw}}} catch {+ y z}}} 1 2 3}"), "5")
    test(run("{+ {try {- 10 {throw}} catch 3} 10}"), "13")
    test(run("{try {if0 0 {throw} {+ 1 2}} catch {if0 10 1 {try {throw} catch 54}}}"), "54")
    test(run("{try {withcc a {+ 1 {withcc b {throw}}}} catch 10}"), "10")
    test(run("{try {- 0 {throw}} catch 5}"), "5")
    test(run("{try {if0 {throw} 3 4} catch 5}"), "5")
    test(run("{try {{fun {x y} {try x catch y}} {throw} 0} catch -1}"), "-1")
    test(run("{{withcc esc {try {{withcc k {try {esc k} catch {fun {x} {fun {y} 9}}}} 0} catch {fun {x} 8}}} {fun {x} {throw}}}"), "8")
    test(run("{withcc foo {{fun {x y} {y x}} {+ 2 3} {withcc bar {+ 1 {bar foo}}}}}"), "5")
    test(run("{try {withcc zzz {{fun {x y z w} {+ {+ x y} {+ z w}}} 1 2 {zzz 10} {throw}}} catch 42}"), "10")
    test(run("{try {withcc zzz {{fun {x y z w} {+ {w {+ x y}} {+ {throw} z}}} 1 2 3 zzz}} catch 42}"), "3")
    test(run("{withcc esc {try {+ {throw} {esc 3}} catch 4}}"), "4")
    test(run("{withcc esc {{fun {x y} {+ {+ x 3} y}} {withcc k {try {k {esc {throw}}} catch {k 5}}} 7}}"), "15")
    test(run("{try {withcc x {+ {x 1} {throw}}} catch 0}"), "1")
    test(run("{+ 12 {withcc k {+ 1 {k {{fun {} 7}}}}}}"), "19")
    test(run("{+ 999 {withcc done {{fun {f x} {f f x done}} {fun {g y z} {if0 {- y 1} {z 100} {+ y {g g {- y 1} z}}}} 10}}}"), "1099")
    test(run("{+ 999 {withcc done {{fun {f x} {f f x {fun {x} {if0 x {done {- 0 999}} 10000}}}} {fun {g y z} {if0 {- y 1} {z 0} {+ y {g g {- y 1} z}}}} 10}}}"), "0")
    test(run("{withcc done {{fun {f x} {f f x {fun {x} {if0 x {fun {y} {fun {x} {+ x y}}} 10000}}}} {fun {g y z} {if0 {- y 1} {z 0} {{g g {- y 1} z} 32}}} 3}}"), "64")
    test(run("{{withcc done {{fun {f x} {f f x {fun {x} {if0 x {withcc k {fun {x} {fun {x} {fun {x} k}}}} 10000}}}} {fun {g y z} {if0 {- y 1} {z 0} {{g g {- y 1} z} 32}}} 3}} 5}"), "continuation")
    test(run("{{withcc done {{fun {f x} {f f x {fun {x} {if0 x {withcc k {fun {x} {fun {x} {fun {x} k}}}} 10000}}}} {fun {g y z} {if0 {- y 1} {z 0} {{g g {- y 1} z} 32}}} 4}} {fun {y} {fun {y} {fun {y} {fun {x} 42}}}}}"), "42")
    test(run("{try {{withcc done {{fun {f x} {f f x {fun {x} {if0 x {withcc k {fun {x} {fun {x} {fun {x} k}}}} {throw}}}}} {fun {g y z} {if0 {- y 1} {z 1} {{g g {- y 1} z} 32}}} 4}} {fun {y} {fun {y} {fun {y} {fun {x} 42}}}}} catch 4242}"), "4242")
    test(run("{withcc esc {{try {withcc done {{fun {f x} {f f x {fun {x} {if0 x {withcc k {fun {x} {fun {x} {fun {x} k}}}} {throw}}}}} {fun {g y z} {if0 {- y 1} {z 1} {{g g {- y 1} z} 32}}} 4}} catch esc} 33}}"), "33")
    test(run("{try {try {throw} catch {try {throw} catch {try {throw} catch {+ {withcc k {try {throw} catch {k 0}}} {throw}}}}} catch 0}"), "0")
    test(run("{try {{withcc done {{fun {f x} {f f x {fun {x} {if0 x {withcc k {fun {x} {fun {x} {fun {x} k}}}} 10000}}}} {fun {g y z} {if0 {- y 1} {z 0} {{g g {- y 1} z} 32}}} 4}} {fun {y} {fun {y} {fun {y} {throw}}}}} catch 4242}"), "4242")
    test(run("{withcc k {+ 1 {k 2}}}"), "2")
    test(run("{withcc done {{withcc esc {done {+ 1 {withcc k {esc k}}}}} 3}}"), "4")
    test(run("{{withcc esc {{withcc k {esc k}} 0}} {fun {x} {fun {y} y}}}"), "function")
    testExc(run("{{fun {x y} 1} 2}"), "wrong arity")
    testExc(run("{throw}"), "no enclosing try-catch")
}
}
