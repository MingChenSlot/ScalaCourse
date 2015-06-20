package kvstore

import akka.actor.{ OneForOneStrategy, Props, ActorRef, Actor }
import kvstore.Arbiter._
import scala.collection.immutable.Queue
import akka.actor.SupervisorStrategy.Restart
import scala.annotation.tailrec
import akka.pattern.{ ask, pipe }
import akka.actor.Terminated
import scala.concurrent.duration._
import akka.actor.PoisonPill
import akka.actor.Cancellable
import akka.actor.OneForOneStrategy
import akka.actor.SupervisorStrategy
import akka.util.Timeout

object Replica {
  sealed trait Operation {
    def key: String
    def id: Long
  }
  case class Insert(key: String, value: String, id: Long) extends Operation
  case class Remove(key: String, id: Long) extends Operation
  case class Get(key: String, id: Long) extends Operation

  sealed trait OperationReply
  case class OperationAck(id: Long) extends OperationReply
  case class OperationFailed(id: Long) extends OperationReply
  case class GetResult(key: String, valueOption: Option[String], id: Long) extends OperationReply

  def props(arbiter: ActorRef, persistenceProps: Props): Props = Props(new Replica(arbiter, persistenceProps))
}

class Replica(val arbiter: ActorRef, persistenceProps: Props) extends Actor {
  import Replica._
  import Replicator._
  import Persistence._
  import context.dispatcher

  /*
   * The contents of this actor is just a suggestion, you can implement it in any way you like.
   */
  override val supervisorStrategy = OneForOneStrategy(maxNrOfRetries = 10, withinTimeRange = 1.second) {
    case _: PersistenceException => Restart
  }
  val persistence = context.actorOf(persistenceProps, "persistence")
  var persistAcks = Map.empty[Long, (ActorRef, Cancellable)]
  var replicateAcks = Map.empty[Long, (ActorRef, Cancellable, Set[ActorRef])]

  var kv = Map.empty[String, String]
  // a map from secondary replicas to replicators
  var secondaries = Map.empty[ActorRef, ActorRef]
  // the current set of replicators
  var replicators = Set.empty[ActorRef]

  // current seq number used by secondary
  var expectSeq = 0L
  var replicationCounter = Long.MaxValue / 2

  def nextReplicationId = {
    if (replicationCounter == Long.MaxValue)
      replicationCounter = Long.MaxValue / 2
    else
      replicationCounter += 1
    replicationCounter
  }

  arbiter ! Join

  def receive = {
    case JoinedPrimary   => context.become(leader)
    case JoinedSecondary => context.become(replica)
  }

  /* TODO Behavior for  the leader role. */
  val leader: Receive = handleGetOperation orElse {
    case Insert(k, v, id) =>
      kv += (k->v)
      replicate(k, Some(v), id, sender)

    case Remove(k, id) =>
      kv -= k
      replicate(k, None, id, sender)

    case Replicated(k, id) =>
      replicateAcks.get(id) match {
        case Some((originator, timeOutSchedule, waitingReplicators)) =>
            replicateAcks += id ->(originator, timeOutSchedule, waitingReplicators - sender)
            updateSelf(id)
        case None =>
      }

    case Persisted(k, id) =>
      persistAcks.get(id) match {
        case Some((originator, schedule)) =>
          schedule.cancel()
          persistAcks -= id
          updateSelf(id)
        case None =>
      }

    case Replicas(replicas) =>
      // Get Replicas that are deprecated
      secondaries = secondaries.filterKeys(replicas)
      val deprecatedReplicators = replicators -- secondaries.values.toSet
      // Remove outstanding acks for depreacted replicators
      replicateAcks foreach {
        case (id, (originator, timeOutSchedule, waitingReplicators)) =>
          replicateAcks += id->(originator, timeOutSchedule, waitingReplicators -- deprecatedReplicators)
          updateSelf(id)
      }
      deprecatedReplicators.foreach (replicator => replicator ! PoisonPill)

      replicators --= deprecatedReplicators

      // Add new joined replica
      val newJoinedReplicas = (replicas -- secondaries.keySet).filterNot(_ == self)

      for (
        newJoinedReplica <- newJoinedReplicas
      ) yield {
        val newReplicator = context.actorOf(Replicator.props(newJoinedReplica))
        secondaries += newJoinedReplica->newReplicator
        replicators += newReplicator
        kv.foreach(x => newReplicator ! Replicate(x._1, Some(x._2), nextReplicationId))
      }
  }

  /* TODO Behavior for the replica role. */
  val replica: Receive = handleGetOperation orElse {
    case Snapshot(k, v, seq) =>
      if (seq == expectSeq) {
        v match {
          case Some(value) => kv += k->value
          case None => kv -= k
        }

        val schedule = context.system.scheduler.schedule(
          0.milliseconds,
          100.milliseconds,
          persistence,
          Persist(k, v, seq))

        persistAcks += seq->(sender, schedule)
        expectSeq += 1
      } else if (seq < expectSeq) {
        sender ! SnapshotAck(k, seq)
      }
      // no action if seq > expectSeq

    case Persisted(k, id) =>
      persistAcks.get(id) match {
        case Some((originator, schedule)) =>
          originator ! SnapshotAck(k, id)
          schedule.cancel()
        case None =>
      }
      persistAcks -= id
  }

  def handleGetOperation: Receive = {
    case Get(k, id) =>
      sender ! GetResult(k, kv.get(k), id)
  }

  def replicate(key: String, valueOption: Option[String], id: Long, originator: ActorRef): Unit = {
    val persistSchedule = context.system.scheduler.schedule(
      0.milliseconds,
      100.milliseconds,
      persistence,
      Persist(key, valueOption, id))

    val timeOutSchedule = context.system.scheduler.scheduleOnce(1.second) {
      originator ! OperationFailed(id)
      replicateAcks -= id
    }

    persistAcks += id->(originator, persistSchedule)

    replicators.foreach(x => x ! Replicate(key, valueOption, id))
    replicateAcks += id->(originator, timeOutSchedule, replicators)
  }

  // check if both persisted and replicated
  def updateSelf(id: Long): Unit = {
    replicateAcks.get(id) match {
      case Some((originator, timeOutSchedule, waitingReplicators)) =>
        if (waitingReplicators.isEmpty && (persistAcks.get(id) == None)) {
          timeOutSchedule.cancel()
          replicateAcks -= id
          originator ! OperationAck(id)
        }
      case None =>
    }
  }
}

