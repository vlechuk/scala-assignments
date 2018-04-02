import patmat.Huffman._

//abstract class Expr {
//  def isNumber: Boolean
//  def isSum: Boolean
//  def numValue: Int
//  def leftOp: Expr
//  def rightOp: Expr
//}
//
//class Number(n: Int) extends Expr {
//  def isNumber: Boolean = true
//  def isSum: Boolean = false
//  def numValue: Int = n
//  def leftOp: Expr = sys.error("Number.leftOp")
//  def rightOp: Expr = sys.error("Number.rightOp")
//}
//
//class Sum(e1: Expr, e2: Expr) extends Expr {
//  def isNumber: Boolean = false
//  def isSum: Boolean = true
//  def numValue: Int = sys.error("Sum.numValue")
//  def leftOp: Expr = e1
//  def rightOp: Expr = e2
//}
//
//def eval(e: Expr): Int = {
//  if (e.isNumber) e.numValue
//  else if (e.isSum) eval(e.leftOp) + eval(e.rightOp)
//  else sys.error("unrecognized expression kind")
//}

trait TestTrees {
  val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
  val t2 = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)
}

class P extends TestTrees {
 val i: Int = weight(t1)
}

val c: P = new P
c.i

val l = Leaf('e', 1)
l.weight
l.char

val r = Leaf('x', 1)

val l2: List[Int] = List(1,2)
l2.head

val sampleTree = makeCodeTree(l,r)
//  Leaf('t', 2)
//)

//sampleTree.chars

