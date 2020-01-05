package cs320

package object hw03 extends Homework03 {
  trait FWAEValue
  case class NumV(n: Int) extends FWAEValue
  case class CloV(param: List[String],
                  body: MRFWAE,
                  env: Env) extends FWAEValue
  case class RecV(rec: Env) extends FWAEValue
  type Env = Map[String, FWAEValue]

  def numOpV(op: (Int, Int) => Int): (FWAEValue, FWAEValue) => FWAEValue = (_, _) match {
    case (NumV(x), NumV(y)) => NumV(op(x, y))
    case (x, y) => error(s"not both numbers: $x, $y")
  }
  val numAddV = numOpV(_ + _)
  val numSubV = numOpV(_ - _)

  def lookup(name: String, env: Env): FWAEValue = env.getOrElse(name, error(s"free identifier: $name"))

  def interpV(params: List[String], args: List[MRFWAE], env: Env, subEnv: Env): Env = {
     params match {
        case Nil => args match {
          case Nil => subEnv
          case v => error("wrong arity")
        }
        case param :: tail => args match {
          case Nil => error("wrong arity")
          case arg :: t => 
              val kv = (param -> interp(arg, env))
              interpV(tail, t, env, subEnv + kv)
        } 
    }
  }

  def interpRecV(rec: Map[String, MRFWAE], env: Env): Env = {
    rec.map{
      case (param, arg) => (param, interp(arg, env))
    }
  }

  def interp(expr: MRFWAE, env: Env): FWAEValue = expr match {
    case Num(nums) => NumV(nums)
    case Add(left, right) => numAddV(interp(left, env), interp(right, env))
    case Id(id) => lookup(id, env)
    case Sub(left, right) => numSubV(interp(left, env), interp(right, env))
    case With(name, expr, body) => interp(body, env + (name -> interp(expr, env)))
    case Fun(params, body) => CloV(params, body, env)
    case App(func, args) => interp(func, env) match {
                                  case CloV(params, body, fenv) => interp(body,fenv ++ interpV(params, args, env, Map()))
                                  case v =>  error(s"not a closure: $v")
                            }
    case Rec(rec) => RecV(interpRecV(rec, env))
    case Acc(expr, name) => interp(expr, env) match {
                              case RecV(rec) => rec.getOrElse(name, error("no such field"))
                              case v => error("no such field")
                            }
  }

  def run(str: String): String = 
    interp(MRFWAE(str), Map()) match {
      case NumV(n) =>  n.toString()
      case RecV(rec) => "record"
      case CloV(param, body, env) => "function"
    }

  def tests: Unit = {
    test(run("{{fun {x y} {+ x y}} 1 2}"), "3")
    test(run("{{fun {} {+ 3 4}}}"), "7")
    testExc(run("{{fun {x y} {+ x y}} 1}"), "wrong arity")
    test(run("{access {record {x 1} {y 2}} x}"), "1")
    testExc(run("{access {record {x 1} {y 2}} z}"), "no such field")
    testExc(run("{access {record {y 1}} z}"), "no such field")
    testExc(run("{record {x {access {record {y 1}} z}}}"), "no such field")
    test(run("42"), "42")
    test(run("{fun {x} x}"), "function")
    test(run("{record {x 1}}"), "record")

    /* Write your own tests */
    test(run("{{{fun {y} {fun {x} {+ y x}}} 10} 5}"),"15")
    test(run("{{{fun {y} {fun {x} {+ {{fun {z} {+ z x}} 15} y}}} 10} 5}"),"30")
    testExc(run("{{{fun {y} {fun {x} {+ {{fun {z v} {+ z x}} 15} y}}} 10} 5}"),"wrong arity")
    test(run("{with {x {fun {y} {access y z}}} {x {record {k 88} {z 2000} {l 88}}}}"), "2000")
    test(run("{fun {x} {+ x y}}"), "function")
    test(run("{{fun {x} {access {record {y 1}} y}} 3}"), "1")
    test(run("{record {a 0} {b 1} {c 2}}"), "record")
    test(run("{record {a {access {record {b 0}} b}} {c 2}}"), "record")
    testExc(run("{record {a {access {record {b 0}} c}} {c 2}}"), "no such field")
  }
}
