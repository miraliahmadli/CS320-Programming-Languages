package cs320

import cs320._

package object hw02 extends Homework02 {
  // applies a binary numeric function on all combinations of numbers from
  // the two input lists, and return the list of all of the results
  def binOp(
    op: (Int, Int) => Int,
    ls: List[Int],
    rs: List[Int]
  ): List[Int] = ls match {
    case Nil => Nil
    case l :: rest =>
      def f(r: Int): Int = op(l,r)
      rs.map(f) ++ binOp(op, rest, rs)
  }

  def min(x: Int , y: Int) : Int = if(x>y) y else x
  def max(x: Int , y: Int) : Int = if(x>y) x else y
  def add(x: Int , y: Int) : Int = x+y
  def sub(x: Int , y: Int) : Int = x-y

  def lookup(name: String, env: Map[String,List[Int]]): List[Int] = env.getOrElse(name, error(s"free identifier: $name"))

  def interp(expr: MUWAE, env: Map[String,List[Int]]) : List[Int] = expr match {
    case Num(nums) => nums
    case Add(left, right) => binOp(add,interp(left, env), interp(right, env))
    case Id(id) => lookup(id, env)
    case Sub(left, right) => binOp(sub,interp(left, env), interp(right, env))
    case With(name, expr, body) => interp(body, env + (name -> interp(expr, env)))
    case Min(left, mid, right) => binOp(min,binOp(min,interp(left,env),interp(mid,env)),interp(right,env))
    case Max(left, mid, right) => binOp(max,binOp(max,interp(left,env),interp(mid,env)),interp(right,env))
  }

  def run(str: String): List[Int] = {
    interp(MUWAE(str), Map())
  }

  def tests: Unit = {
    test(run("{+ 3 7}"), List(10))
    test(run("{- 10 {3 5}}"), List(7, 5))
    test(run("{with {x {+ 5 5}} {+ x x}}"), List(20))
    test(run("{min 3 4 5}"), List(3))
    test(run("{max {+ 1 2} 4 5}"), List(5))
    test(run("{min {1 4} {2 9} 3}"), List(1, 1, 2, 3))
    test(run("{max {1 6} {2 5} {3 4}}"), List(3, 4, 5, 5, 6, 6, 6, 6))

    /* Write your own tests */
    test(run("{max 1 2 3}"),List(3))
    test(run("{min 1 2 3}"),List(1))

    test(binOp(add, List(1,2), List(5,-5)), List(6, -4, 7, -3))

    test(run("{- {max {1 7} {2 6} {3 5}} {4}}"),List(-1, 1, 2, 2, 3, 3, 3, 3))
    test(run("{with {x {min {1 2} {3 4} 5}} {+ x x}}"), List(2, 2, 3, 3, 2, 2, 3, 3, 3, 3, 4, 4, 3, 3, 4, 4))
    test(run("{with {x {+ {1 2} {1 2}}} {max x {1 5} 3}}"), List(3, 5, 3, 5, 3, 5, 4, 5))

    test(run("{with {x {}} {max x {1 5} 3}}"), List())
    testExc(run("x"), "free identifier: x")
    testExc(run("{with {Ahmadli 2000} Mirali}"), "free identifier: Mirali")
  }
}
