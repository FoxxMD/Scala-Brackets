import BracketTree.BST
import org.scalatest.{Matchers, FlatSpec}

/**
 * Created by Matthew on 11/25/2014.
 */
class BSTTest extends FlatSpec with Matchers {

  val bst = BST(12) + 6 + 4 + 2 + 1 + 3 ++ BST(5, 8, 7, 10, 9)
  def f(x: Int) = BST(x + 1)
  def g(x: Int) = BST(x + 2)

  "BST" should "contain the Int 5" in {
    bst.contains(5)
  }
  it should "be true that 5 exists" in {
    bst.exists(_ == 5) should be (true)
  }
  it should "be true that 11 does not exist"  in {
    bst.exists(_ == 11) should not be true
  }
  it should "filter all even values" in {
    bst.filter(_ % 2 == 0).toList should be (List(12, 6, 4, 2, 8, 10))
  }

  "BST" should "remove the value 6" in {
    val bstNew = BST(12) + 4 + 2 + 1 + 3 ++ BST(5, 8, 7, 10, 9)
    val (opt, bst1) = bst - 6
    opt should be (Some(6))
    bst1.exists(_ == 6) should not be true
  }

  behavior of "BST is a monad"

  it should "have map and flatMap equivalence" in {
    bst.map(_ + 1) should be (bst.flatMap(x => BST(x + 1)))
  }
  it should "have associativity" in {
    bst.flatMap(f).flatMap(g) should be (bst.flatMap(x => f(x).flatMap(g)))
  }
  it should "left unit" in {
    BST(1).flatMap(f) should be (f(1))
  }
  it should "right unit" in {
    bst.flatMap(BST(_)) should be (bst)
  }

}
