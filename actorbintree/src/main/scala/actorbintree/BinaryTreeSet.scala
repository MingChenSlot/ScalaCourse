/**
 * Copyright (C) 2009-2013 Typesafe Inc. <http://www.typesafe.com>
 */
package actorbintree

import akka.actor._
import scala.collection.immutable.Queue

object BinaryTreeSet {

  trait Operation {
    def requester: ActorRef
    def id: Int
    def elem: Int
  }

  trait OperationReply {
    def id: Int
  }

  /** Request with identifier `id` to insert an element `elem` into the tree.
    * The actor at reference `requester` should be notified when this operation
    * is completed.
    */
  case class Insert(requester: ActorRef, id: Int, elem: Int) extends Operation

  /** Request with identifier `id` to check whether an element `elem` is present
    * in the tree. The actor at reference `requester` should be notified when
    * this operation is completed.
    */
  case class Contains(requester: ActorRef, id: Int, elem: Int) extends Operation

  /** Request with identifier `id` to remove the element `elem` from the tree.
    * The actor at reference `requester` should be notified when this operation
    * is completed.
    */
  case class Remove(requester: ActorRef, id: Int, elem: Int) extends Operation

  /** Request to perform garbage collection*/
  case object GC

  case object Traverse

  /** Holds the answer to the Contains request with identifier `id`.
    * `result` is true if and only if the element is present in the tree.
    */
  case class ContainsResult(id: Int, result: Boolean) extends OperationReply

  /** Message to signal successful completion of an insert or remove operation. */
  case class OperationFinished(id: Int) extends OperationReply

}


class BinaryTreeSet extends Actor {
  import BinaryTreeSet._
  import BinaryTreeNode._

  def createRoot: ActorRef = context.actorOf(BinaryTreeNode.props(0, initiallyRemoved = true))

  var root = createRoot

  var pendingQueue = Queue.empty[Operation]

  def receive = normal

  /** Accepts `Operation` and `GC` messages. */
  val normal: Receive = {
    case insert: Insert => root ! insert
    case remove: Remove => root ! remove
    case contains: Contains => root ! contains
    case GC => {
      val newRoot = createRoot
      root ! CopyTo(newRoot)
      context.become(garbageCollecting(newRoot))
    }
    case Traverse => root ! Traverse
  }

  /** Handles messages while garbage collection is performed.
    * `newRoot` is the root of the new binary tree where we want to copy
    * all non-removed elements into.
    */
  def garbageCollecting(newRoot: ActorRef): Receive = {
    case GC => None

    case operation: Operation => {
      pendingQueue = pendingQueue.enqueue(operation)
    }

    case CopyFinished => {
      root ! PoisonPill
      root = newRoot

      pendingQueue.foreach(root ! _)
      pendingQueue = Queue.empty[Operation]
      context.become(normal)
    }
  }

}

object BinaryTreeNode {
  trait Position

  case object Left extends Position
  case object Right extends Position

  case class CopyTo(treeNode: ActorRef)
  case object CopyFinished

  def props(elem: Int, initiallyRemoved: Boolean) = Props(classOf[BinaryTreeNode],  elem, initiallyRemoved)
}

class BinaryTreeNode(val elem: Int, initiallyRemoved: Boolean) extends Actor {
  import BinaryTreeNode._
  import BinaryTreeSet._

  var subtrees = Map[Position, ActorRef]()
  var removed = initiallyRemoved

  val selfCopyID = -100

  def receive = normal

  def createNode(value: Int): ActorRef = {
    context.actorOf(BinaryTreeNode.props(value, false))
  }

  /** Handles `Operation` messages and `CopyTo` requests. */
  val normal: Receive = {
    case Insert(requester, id, value) => {
      if (value == elem) {
        if (removed) removed = false
        requester ! OperationFinished(id)
      } else if (value < elem) {
        subtrees.get(Left) match {
          case Some(left) => left ! Insert(requester, id, value)
          case None => {
            subtrees += Left -> createNode(value)
            requester ! OperationFinished(id)
          }
        }
      } else {
        subtrees.get(Right) match {
          case Some(right) => right ! Insert(requester, id, value)
          case None => {
            subtrees += Right -> createNode(value)
            requester ! OperationFinished(id)
          }
        }
      }
    }

    case Remove(requester, id, value) => {
      if (value == elem) {
        removed = true
        requester ! OperationFinished(id)
      } else if (value < elem) {
        subtrees.get(Left) match {
          case Some(left) => left ! Remove(requester, id, value)
          case None =>
            requester ! OperationFinished(id)
        }
      } else {
        subtrees.get(Right) match {
          case Some(right) => right ! Remove(requester, id, value)
          case None =>
            requester ! OperationFinished(id)
        }
      }
    }

    case Contains(requester, id, value) => {
      if (value == elem) {
        if (removed) {
          requester ! ContainsResult(id, false)
        } else {
          requester ! ContainsResult(id, true)
        }
      } else if (value < elem) {
        subtrees.get(Left) match {
          case Some(left) => left ! Contains(requester, id, value)
          case None => requester ! ContainsResult(id, false)
        }
      } else {
        subtrees.get(Right) match {
          case Some(right) => right ! Contains(requester, id, value)
          case None => requester ! ContainsResult(id, false)
        }
      }
    }

    case CopyTo(newRoot) => {
      val expected = subtrees.values.toSet
      expected.foreach(_ ! CopyTo(newRoot))

      if (!removed) {
        newRoot ! Insert(self, selfCopyID, elem)
      }

      if (removed && expected.isEmpty) {
        sender ! CopyFinished
      } else {
        context.become(copying(expected, removed, sender))
      }
    }

    case Traverse => {
      println("Value:" + elem + " removed:" + removed)
      subtrees.get(Left) match {
        case Some(left) => left ! Traverse
        case None =>
      }
      subtrees.get(Right) match {
        case Some(right) => right! Traverse
        case None =>
      }
    }
  }

  /** `expected` is the set of ActorRefs whose replies we are waiting for,
    * `insertConfirmed` tracks whether the copy of this node to the new tree has been confirmed.
    */
  def copying(expected: Set[ActorRef], insertConfirmed: Boolean, originator: ActorRef): Receive = {
    case OperationFinished(selfCopyID) => {
      if (expected.isEmpty) {
        originator ! CopyFinished
      } else {
        context.become(copying(expected, true, originator))
      }
    }

    case CopyFinished => {
      val newExpected = expected - sender
      if (newExpected.isEmpty && insertConfirmed) {
        originator ! CopyFinished
      } else {
        context.become(copying(newExpected, insertConfirmed, originator))
      }
    }
  }
}


