package day23

import scala.annotation.tailrec

case class Node[A](value: A, var next: Node[A]) {
  override def toString: String = s"Node($value)"
}

object Node {
  def createSelfReferencing[A](a: A): Node[A] = {
    val n = Node(a, null)
    n.next = n
    n
  }

}

case class CircularList[A] private (private var maybeTail: Option[Node[A]], private var map: collection.mutable.HashMap[A, Node[A]]) {

  def tail: Option[Node[A]] = maybeTail

  def append(a: A): Node[A] = {

    val n = maybeTail match {
      case Some(currentTail) =>
        val header = currentTail.next
        val n = Node(a, header)
        currentTail.next = n
        n
      case None =>
        //we're empty right now, so create a node with a pointer to itself
        Node.createSelfReferencing(a)

    }

    map.update(a, n)
    maybeTail = Some(n)
    n
  }

  def unlinkAfter(a: A, n: Int): CircularList[A] = {

    find(a) match {
      case None => CircularList.create[A]()
      case Some(node) =>
        val length = map.size
        val numberToRemove = n % length

        take(numberToRemove, Some(node.next)) match {
          case Some((lastRemovedNode, removed)) =>
            val lastNodeNext = lastRemovedNode.next
            lastRemovedNode.next = removed.head
            node.next = lastNodeNext
            val newTail = maybeTail.map { tail =>
              if (removed.contains(tail)) node else tail
            }
            val newMap = collection.mutable.HashMap.empty[A, Node[A]]
            removed.foreach { n =>
              map.remove(n.value)
              newMap.update(n.value, n)
            }
            maybeTail = newTail
            new CircularList[A](Some(lastRemovedNode), newMap)

          case None =>
            CircularList.create[A]()
        }
    }
  }

  def take(n: Int, entrypoint: Option[Node[A]] = None): Option[(Node[A], List[Node[A]])] = {

    @tailrec
    def helper(n: Int, current: Node[A], acc: List[Node[A]]): Option[(Node[A], List[Node[A]])] = {
      if (n == 1) Some((current, (current :: acc).reverse))
      else helper(n - 1, current.next, current :: acc)
    }

    entrypoint.orElse(maybeTail.map(_.next)) match {
      case Some(value) => helper(n, value, List.empty)
      case None        => None
    }
  }

  def allElements: List[Node[A]] = {
    maybeTail match {
      case Some(tail) => takeWhile(tail.next, n => n != tail)
      case None       => List.empty
    }
  }

  def takeWhile(current: Node[A], cond: Node[A] => Boolean): List[Node[A]] = {

    @tailrec
    def helper(current: Node[A], acc: List[Node[A]]): List[Node[A]] = {
      if (cond(current)) {
        helper(current.next, current :: acc)
      } else {
        (current :: acc).reverse
      }
    }
    helper(current, List.empty)
  }

  def find(value: A): Option[Node[A]] =
    map.get(value)

}

object CircularList {

  def create[A]() = new CircularList[A](None, collection.mutable.HashMap.empty)

}
