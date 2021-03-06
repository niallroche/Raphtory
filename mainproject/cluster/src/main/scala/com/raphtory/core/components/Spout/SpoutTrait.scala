package com.raphtory.core.components.Spout

import akka.actor.Actor
import akka.actor.ActorLogging
import akka.actor.Cancellable
import akka.actor.Timers
import akka.cluster.pubsub.DistributedPubSub
import akka.cluster.pubsub.DistributedPubSubMediator
import akka.pattern.ask
import akka.util.Timeout
import com.raphtory.core.model.communication._
import com.raphtory.core.utils.SchedulerUtil
import kamon.Kamon
//import kamon.metric.CounterMetric
//import kamon.metric.GaugeMetric

import scala.collection.mutable
import scala.concurrent.Await
import scala.concurrent.duration.Duration
import scala.concurrent.duration._
import scala.language.postfixOps

// TODO Add val name which sub classes that extend this trait must overwrite
//  e.g. BlockChainSpout val name = "Blockchain Spout"
//  Log.debug that read 'Spout' should then read 'Blockchain Spout'
trait SpoutTrait extends Actor with ActorLogging with Timers {
  case class StartSpout()
  private var safe            = false
  implicit val executionContext = context.system.dispatchers.lookup("spout-dispatcher")

  private val scheduledTaskMap: mutable.HashMap[String, Cancellable] = mutable.HashMap[String, Cancellable]()
  val spoutTuples       = Kamon.counter("Raphtory_Spout_Tuples").withTag("actor",self.path.name)
  var count = 0
  protected def recordUpdate(): Unit = {
    spoutTuples.increment()
    count += 1
  }

  final protected val mediator = DistributedPubSub(context.system).mediator
  mediator ! DistributedPubSubMediator.Put(self)

  override def preStart() {
    log.debug("Spout is being started.")
    scheduleTasks()
  }

  override def postStop() {
    val allTasksCancelled = scheduledTaskMap.forall {
      case (key, task) =>
        SchedulerUtil.cancelTask(key, task)
    }
    if (!allTasksCancelled) log.warning("Failed to cancel all scheduled tasks post stop.")
  }

  final override def receive: Receive = {
    case msg: String if msg == "stateCheck" => processStateCheckMessage(msg)
    case msg: String if msg == "isSafe"     => processIsSafeMessage(msg)
    case x                                  => ProcessSpoutTask(x) // TODO How do we know this is a spout task? Add a trait which spout messages extend
  }

  def start(): Unit = safe = true

  def stop(): Unit = safe = false

  protected def ProcessSpoutTask(receivedMessage: Any)

  private def processStateCheckMessage(msg: String): Unit = {
    log.debug(s"Spout received [{}] message.", msg)

    if (!safe)
      try {
        implicit val timeout: Timeout = Timeout(10 seconds)

        val sendMessage = ClusterStatusRequest()
        val sendPath    = "/user/WatchDog"

        log.debug(s"Sending DPSM message [{}] to path [{}].", sendMessage, sendPath)

        val future =
          mediator ? DistributedPubSubMediator.Send(sendPath, sendMessage, localAffinity = false)

        safe = Await.result(future, timeout.duration).asInstanceOf[ClusterStatusResponse].clusterUp

      } catch { case _: java.util.concurrent.TimeoutException => safe = false }
  }

  private def processIsSafeMessage(msg: String): Option[Cancellable] = {
    log.debug(s"Spout received [{}] message.", msg)

    if (safe) {
      val startSpoutCancellable =
        SchedulerUtil.scheduleTaskOnce(delay = 1 millisecond, receiver = self, message = StartSpout)
      scheduledTaskMap.put("startSpout", startSpoutCancellable)
    } else {
      val isSafeCancellable = SchedulerUtil.scheduleTaskOnce(delay = 1 second, receiver = self, message = "isSafe")
      scheduledTaskMap.put("isSafe", isSafeCancellable)
    }
  }

  def AllocateSpoutTask(duration: Duration, task: Any): Cancellable = {
    val taskCancellable = SchedulerUtil.scheduleTaskOnce(Duration(duration._1, duration._2), self, task)

    // TODO
    //  scheduledTaskMap.put("passNameAsArgument", taskCancellable)

    taskCancellable
  }

  protected def sendTuple(command: String): Unit = {
    log.debug("The command [{}] received for send.", command)
    recordUpdate()
    if(count%100==0)
      mediator ! DistributedPubSubMediator.Send(s"/user/router/routerWorker_${count % 10}", AllocateTrackedTuple(System.currentTimeMillis(),command), localAffinity = false)
    else
      mediator ! DistributedPubSubMediator.Send(s"/user/router/routerWorker_${count % 10}", AllocateTuple(command), localAffinity = false)  }

  protected def sendTuple[T <: SpoutGoing](command: T): Unit = {
    log.debug("The command [{}] received for send.", command)

    recordUpdate()
    if(count%100==0)
      mediator ! DistributedPubSubMediator.Send(s"/user/router/routerWorker_${count % 10}", AllocateTrackedTuple(System.currentTimeMillis(),command), localAffinity = false)
    else
      mediator ! DistributedPubSubMediator.Send(s"/user/router/routerWorker_${count % 10}", AllocateTuple(command), localAffinity = false)
  }


  private def scheduleTasks(): Unit = {
    log.debug("Preparing to schedule tasks in Spout.")

    val stateCheckCancellable =
      SchedulerUtil.scheduleTask(initialDelay = 7 seconds, interval = 1 second, receiver = self, message = "stateCheck")
    scheduledTaskMap.put("stateCheck", stateCheckCancellable)

    val isSafeCancellable =
      SchedulerUtil.scheduleTaskOnce(delay = 1 seconds, receiver = self, message = "isSafe")
    scheduledTaskMap.put("isSafe", isSafeCancellable)
  }
}
