package kvstore

import akka.actor.Props
import akka.actor.Actor
import akka.actor.ActorRef
import akka.actor.Cancellable
import scala.concurrent.duration._

object Replicator {
  case class Replicate(key: String, valueOption: Option[String], id: Long)
  case class Replicated(key: String, id: Long)
  
  case class Snapshot(key: String, valueOption: Option[String], seq: Long)
  case class SnapshotAck(key: String, seq: Long)

  case object Flushing

  def props(replica: ActorRef): Props = Props(new Replicator(replica))
}

class Replicator(val replica: ActorRef) extends Actor {
  import Replicator._
  import Replica._
  import context.dispatcher
  
  /*
   * The contents of this actor is just a suggestion, you can implement it in any way you like.
   */

  // map from sequence number to tuple of sender, request and schedule
  var acks = Map.empty[Long, (ActorRef, Replicate, Cancellable)]
  // a sequence of not-yet-sent snapshots (you can disregard this if not implementing batching)
  // batch by: key=>(sender, Snapshot, id)
  var pending = Map.empty[String, (ActorRef, Snapshot, Long)]
  val flushSchedule = context.system.scheduler.schedule(
    50.milliseconds,
    50.milliseconds,
    self,
    Flushing)
  
  var _seqCounter = 0L
  def nextSeq = {
    val ret = _seqCounter
    _seqCounter += 1
    ret
  }

  override def postStop(): Unit = {
    flushSchedule.cancel()
    acks.values foreach {
      case (_, _, schedule) => schedule.cancel()
    }
  }

  /* TODO Behavior for the Replicator. */
  def receive: Receive = {
    case Replicate(k, v, id) =>
      val seq = nextSeq
      pending += k->(sender, Snapshot(k, v, seq), id)

    case SnapshotAck(k, seq) =>
      acks.get(seq) match {
        case Some((originator, Replicate(k, v, id), schedule)) =>
          schedule.cancel()
          acks -= seq
          originator ! Replicated(k, id)
        case None =>
      }

    case Flushing =>
      pending.foreach {case (k, (originator, snapshot @ Snapshot(key, v, seq), id)) =>
        val schedule = context.system.scheduler.schedule(
          0.milliseconds,
          100.milliseconds,
          replica,
          snapshot)
        acks += seq->(originator, Replicate(k, v, id), schedule)
      }
  }

}
