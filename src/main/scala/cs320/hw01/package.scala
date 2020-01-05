package cs320

package object hw01 extends Homework01 {
  // 1. Primitives (20 pts)
  def volumeOfCuboid(a: Int, b: Int, c: Int): Int = 
      a*b*c
  def concat(x: String, y: String): String = 
      x+y
  // 2. Function Values (30 pts)
  def addN(n: Int): Int => Int = {
      def f(x: Int) : Int = x+n
      f
  }
  def twice(f: Int => Int): Int => Int = {
      def g(x : Int) : Int = f(f(x))
      g
  }

  def compose(f: Int => Int, g: Int => Int): Int => Int = {
      def h(x : Int) : Int = f(g(x))
      h
  }

  // 3. Data Structures (50 pts)
  // 3.1. Lists (20 pts)
  def double(l: List[Int]): List[Int] = l.map(_ * 2)
  def sum(l: List[Int]): Int = l.foldLeft(0)(_ + _)

  // 3.2. Maps (10 pts)
  def getKey(m: Map[String, Int], s: String): Int = if (m.contains(s)) m(s) else error[Int](s)

  // 3.3. User-defined Structures (20 pts)
  def countLeaves(t: Tree): Int = t match {
    case Leaf(value) => 1
    case Branch(left, value, right) => countLeaves(left) + countLeaves(right)
  }
  def flatten(t: Tree): List[Int] = t match {
    case Leaf(value) => value :: Nil
    case Branch(left, value, right) =>  flatten(left) ++ (value :: flatten(right))
  }

  def tests: Unit = {
    test(concat("abc", "def"), "abcdef")
    test(addN(5)(3), 8)
    test(addN(5)(42), 47)
    test(twice(addN(3))(2), 8)
    test(twice(addN(3))(7), 13)
    test(compose(addN(3), addN(4))(5), 12)
    test(compose(addN(3), addN(4))(11), 18)

    val l: List[Int] = List(1, 2, 3)
    test(double(l), List(2, 4, 6))
    test(double(double(l)), List(4, 8, 12))

    test(sum(List(1,2,3)), 6)
    test(sum(List(4,2,3,7,5)), 21)

    val m: Map[String, Int] = Map("Ryu" -> 42, "PL" -> 37)
    test(getKey(m, "Ryu"), 42)
    test(getKey(m, "PL"), 37)
    testExc(getKey(m, "CS320"), "CS320")

    val tree: Tree = Branch(Leaf(1), 2, Branch(Leaf(3), 4, Leaf(5)))
    test(countLeaves(tree), 3)
    test(flatten(tree), List(1, 2, 3, 4, 5))

    /* Write your own tests */
    test(volumeOfCuboid(1000,1000,1000), 1000000000)
    test(volumeOfCuboid(1,100,0), 0)
    test(volumeOfCuboid(50,30,40), 60000)

    test(concat("mirali", "ahmadli"), "miraliahmadli")
    test(concat("", ""), "")

    val f: Int => Int = addN(11)
    test(f(9), 20)
    test(f(4200), 4211)

    val g: Int => Int = twice(f)
    test(g(6), 28)
    test(g(2200), 2222)
    
    val h: Int => Int = compose(f, twice(addN(42)))
    test(h(5), 100)
    test(h(4800), 4895)

    val lst: List[Int] = List.empty[Int]
    test(double(lst), List.empty[Int])
    
    val lst1: List[Int] = List(11, 44, 176)
    test(double(double(lst1)), List(44, 176, 704))

    test(sum(List(1, -2, 3, -4, 5, -6, 7)), 4)
    test(sum(lst), 0)

    val map: Map[String, Int] = Map("ID" -> 20170847, "Year" -> 2000, "Month" -> 12, "Day" -> 9)
    test(getKey(map, "ID"), 20170847)
    test(getKey(map, "Year"), 2000)
    test(getKey(map, "Month"), 12)
    test(getKey(map, "Day"), 9)
    testExc(getKey(map, "Age"), "Age")

    val tree1: Tree = Branch(Branch(Leaf(1), 2, Branch(Leaf(3), 4, Leaf(5))), 6, Branch(Branch(Leaf(7), 8, Leaf(9)), 10, Leaf(11)))
    test(countLeaves(tree1), 6)
    test(flatten(tree1), List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11))
  }
}
