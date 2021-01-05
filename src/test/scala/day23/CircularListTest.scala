package day23
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class CircularListTest extends AnyFunSuite with Matchers {

  test("creating an empty list should have no tail") {
    val l = CircularList.create[Int]()
    l.tail shouldBe None
  }
  test("appending to an empty list should have the appended node as its tail") {
    val l = CircularList.create[Int]()
    val appended = l.append(1)

    l.tail shouldBe Some(appended)
  }

  test("appending twice should have 2nd node as its tail") {
    val l = CircularList.create[Int]()
    val _ = l.append(1)
    val appended2 = l.append(2)

    l.tail shouldBe Some(appended2)
  }

  test("appending twice should have 1st.next == 2nd") {
    val l = CircularList.create[Int]()
    val appended1 = l.append(1)
    val appended2 = l.append(2)

    appended1.next shouldBe appended2
  }

  test("taking from an empty list should yield List.empty") {
    val l = CircularList.create[Int]()
    l.take(3) shouldBe None
  }

  test("taking from a list with n = 1 should yield the one element n times") {
    val l = CircularList.create[Int]()
    val n1 = l.append(1)

    l.take(3).map(_._2.map(_.value)) shouldBe Some(List(1, 1, 1))
  }

  test("taking from a list with n = 2 should alternate the elements") {
    val l = CircularList.create[Int]()
    l.append(1)
    l.append(2)
    l.take(3).map(_._2.map(_.value)) shouldBe Some(List(1, 2, 1))
  }

  test("find should yield a valid pointer to a node") {
    val l = CircularList.create[Int]()
    l.append(1)
    val n2 = l.append(2)
    val n3 = l.append(3)

    l.find(2) shouldBe Some(n2)
    l.find(2).map(_.next) shouldBe Some(n3)
  }

  test("take from a list with a valid entry point should have this entrypoint at the end of the list") {
    val l = CircularList.create[Int]()
    l.append(1)
    l.append(2)
    val n3 = l.append(3)
    l.append(4)
    l.append(5)

    l.take(5, Some(n3)).map(_._2.map(_.value)) shouldBe Some(List(3, 4, 5, 1, 2))
  }

  test("allElements should give all elements from head to tail ") {
    val l = CircularList.create[Int]()
    val n2 = l.append(2)
    val n1 = l.append(1)
    val n3 = l.append(3)
    l.allElements.map(_.value) shouldBe List(2, 1, 3)
  }

  test("remove should update state of the world accordingly") {
    val l = CircularList.create[Int]()
    l.append(1)
    l.append(2)
    l.append(3)

    val unlinkedList = l.unlinkAfter(a = 1, n = 1)
    val actual = l.allElements.map(_.value)

    actual shouldBe List(1, 3)
    unlinkedList.allElements.map(_.value) shouldBe List(2)

  }

  test("remove 1 element from n-1 should update the tail") {
    val l = CircularList.create[Int]()
    l.append(1)
    l.append(2)
    l.append(3)
    l.append(4)

    val unlinkedList = l.unlinkAfter(a = 3, n = 1)
    l.allElements.map(_.value) shouldBe List(1, 2, 3)
    unlinkedList.allElements.map(_.value) shouldBe List(4)
  }

  test("remove 2 elements from n-1 should update both head and tail") {
    val l = CircularList.create[Int]()
    l.append(1) // will be removed
    l.append(2)
    l.append(3)
    l.append(4) // will be removed

    val unlinkedList = l.unlinkAfter(a = 3, n = 2)
    l.allElements.map(_.value) shouldBe List(2, 3)
    unlinkedList.allElements.map(_.value) shouldBe List(4, 1)
  }

}
