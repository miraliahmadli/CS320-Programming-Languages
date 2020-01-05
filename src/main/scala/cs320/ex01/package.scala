
package cs320

package object ex01 extends Exercise01 {
  // Problem 1
  def freeIds(expr: WAE): Set[String] = {
    def frees(e: WAE, ids: Set[String]): Set[String] = e match {
      case Num(n) => Set()
      case Add(left, right) => frees(left, ids) ++ frees(right, ids)
      case Sub(left, right) => frees(left, ids) ++ frees(right, ids)
      case With(name, expr, body) => frees(expr, ids) ++ frees(body, ids + name)
      case Id(id) =>
        if (ids contains id) Set()
        else Set(id)
    }
    frees(expr, Set())
  }

  // Problem 2
  def bindingIds(expr: WAE): Set[String] = expr match {
    case Num(n) => Set()
    case Add(left, right) => bindingIds(left) ++ bindingIds(right)
    case Sub(left, right) => bindingIds(left) ++ bindingIds(right)
    case With(name, expr, body) => bindingIds(expr) ++ bindingIds(body) + name
    case Id(id) => Set()
  }

  // Problem 3
  def boundIds(expr: WAE): Set[String] = {
    def bounds(e: WAE, ids: Set[String]): Set[String] = e match {
      case Num(n) => Set()
      case Add(left, right) => bounds(left, ids) ++ bounds(right, ids)
      case Sub(left, right) => bounds(left, ids) ++ bounds(right, ids)
      case With(name, expr, body) => bounds(expr, ids) ++ bounds(body, ids + name)
      case Id(id) =>
        if (ids contains id) Set(id)
        else Set()
    }
    bounds(expr, Set())
  }

  // Tests
  def tests: Unit = {
    // tests for freeIds
    test(freeIds(WAE("{with {x 3} {+ x {- 3 x}}}")), Set())
    test(freeIds(WAE("{with {x 3} {- a {+ 4 x}}}")), Set("a"))
    test(freeIds(WAE("{with {x 3} {- b {- a x}}}")), Set("a", "b"))
    test(freeIds(WAE("{with {x 3} {- a {- b {+ x b}}}}")), Set("a", "b"))
    test(freeIds(WAE("{with {x 3} {- y {with {y 7} {+ x {- b a}}}}}")), Set("a", "b", "y"))
    test(freeIds(WAE("{with {x t} {- x {with {y y} {+ x {- b a}}}}}")), Set("a", "b", "t", "y"))
    test(freeIds(WAE("{with {x {with {y 3} {- x y}}} {+ x y}}")), Set("x", "y"))
    test(freeIds(WAE("{+ {with {x 10} {with {x 3} {- y {with {y 7} {+ x {- c b}}}}}} {with {a a} a}}")), Set("a", "b", "c", "y"))
    test(freeIds(WAE("{+ {with {x 10} {with {x 3} {- y {with {y 7} {+ x {- c b}}}}}} {with {a d} a}}")), Set("b", "c", "d", "y"))
    test(freeIds(WAE("{+ {with {x 10} {with {x 3} {- y {with {y 7} {+ x {- c b}}}}}} {with {a d} z}}")), Set("b", "c", "d", "y", "z"))

    // tests for bindingIds
    test(bindingIds(WAE("{+ 3 {- x y}}")), Set())
    test(bindingIds(WAE("{with {y 3} {with {x x} y}}")), Set("x", "y"))
    test(bindingIds(WAE("{with {y 3} {with {y x} {+ x y}}}")), Set("y"))
    test(bindingIds(WAE("{with {y 3} {with {y {with {x {+ 3 y}} {- x y}}} {+ x y}}}")), Set("x", "y"))
    test(bindingIds(WAE("{with {z 3} {with {w {with {z {+ 3 y}} {- x y}}} {with {w y} {+ 7 w}}}}")), Set("w", "z"))

    // tests for boundIds
    test(boundIds(WAE("{with {x 3} {+ y 3}}")), Set())
    test(boundIds(WAE("{with {x 3} {+ x {- x y}}}")), Set("x"))
    test(boundIds(WAE("{with {x 3} {+ x {with {y 7} {- x y}}}}")), Set("x", "y"))
    test(boundIds(WAE("{with {x 3} {with {y x} {- 3 y}}}")), Set("x", "y"))
    test(boundIds(WAE("{with {x 3} {+ y {with {y x} {- 3 7}}}}")), Set("x"))
    test(boundIds(WAE("{with {x x} {+ y {with {y y} {- 3 {with {z 7} {- z x}}}}}}")), Set("x", "z"))
    test(boundIds(WAE("{with {x {with {y 3} {+ x y}}} {+ y {with {y y} {- 3 7}}}}")), Set("y"))
    test(boundIds(WAE("{with {x a} {with {y b} {with {z c} {+ d {- x {+ y z}}}}}}")), Set("x", "y", "z"))
    test(boundIds(WAE("{+ {with {x 10} {with {x 3} {- y {with {y 7} {+ x {- c b}}}}}} {with {a d} a}}")), Set("a", "x"))
    test(boundIds(WAE("{+ {with {x 10} {with {x 3} {- y {with {y 7} {+ x {- c b}}}}}} {with {a d} z}}")), Set("x"))
  }
}

