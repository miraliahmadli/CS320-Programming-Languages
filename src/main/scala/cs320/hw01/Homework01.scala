package cs320

trait Homework01 extends Homework {
  // 1. Primitives (20 pts)
  def volumeOfCuboid(a: Int, b: Int, c: Int): Int
  def concat(x: String, y: String): String

  // 2. Function Values (30 pts)
  def addN(n: Int): Int => Int
  def twice(f: Int => Int): Int => Int
  def compose(f: Int => Int, g: Int => Int): Int => Int

  // 3. Data Structures (50 pts)
  // 3.1. Lists (20 pts)
  def double(l: List[Int]): List[Int]
  def sum(l: List[Int]): Int

  // 3.2. Maps (10 pts)
  def getKey(m: Map[String, Int], s: String): Int

  // 3.3. User-defined Structures (20 pts)
  trait Tree
  case class Branch(left: Tree, value: Int, right: Tree) extends Tree
  case class Leaf(value: Int) extends Tree
  def countLeaves(t: Tree): Int
  def flatten(t: Tree): List[Int]
}
