package com.raphtory.spouts

import java.util
import java.util.Properties

import com.raphtory.core.components.Spout.SpoutTrait
import org.apache.kafka.clients.consumer.KafkaConsumer

import scala.collection.JavaConverters._
import scala.concurrent.duration.Duration
import scala.concurrent.duration.MILLISECONDS
import scala.concurrent.duration.SECONDS
import java.util.concurrent.LinkedBlockingQueue

import akka.actor.Props
import com.raphtory.core.components.Router.RouterManager

import scala.concurrent.duration.Duration
import scala.concurrent.duration._
import com.raphtory.core.utils.SchedulerUtil

import scala.util.Random
class KafkaSpout extends SpoutTrait {
  println("Starting kafka")
  var kafkaServer     = System.getenv().getOrDefault("KAFKA_ADDRESS", "127.0.0.1").trim
  var kafkaIP     = System.getenv().getOrDefault("KAFKA_PORT", "9092").trim
  var offset     = System.getenv().getOrDefault("KAFKA_OFFSET", "earliest").trim
  val x     = new Random().nextLong()
  var groupID   = System.getenv().getOrDefault("KAFKA_GROUP", "group" + x).trim
  var topic   = System.getenv().getOrDefault("KAFKA_TOPIC", "sample_topic").trim
  var restart   = System.getenv().getOrDefault("RESTART_RATE", "10").trim

  val queue = new LinkedBlockingQueue[String]

  val props = new Properties()
  props.put("bootstrap.servers", s"$kafkaServer:$kafkaIP")
  props.put("key.deserializer", "org.apache.kafka.common.serialization.StringDeserializer")
  props.put("value.deserializer", "org.apache.kafka.common.serialization.StringDeserializer")
  props.put("auto.offset.reset", offset)
  props.put("group.id", groupID)
  val consumer: KafkaConsumer[String, String] = new KafkaConsumer[String, String](props)
  consumer.subscribe(util.Arrays.asList(topic))


  val helper = context.system.actorOf(Props(new KafkaSpoutBackPressure(queue)), "Spout_Helper")

  protected def ProcessSpoutTask(message: Any): Unit = message match {
    case StartSpout => AllocateSpoutTask(Duration(1, MILLISECONDS), "newLine")
    case "newLine"  => consumeFromKafka()
    case _          => println("message not recognized!")
  }

  def consumeFromKafka() = {
    //println("Consuming")
    val record = consumer.poll(java.time.Duration.ofMillis(3000)).asScala
    for (data <- record.iterator) {
      //sendTuple(data.value())
      helper ! KafkaData(data.value())
    }
    AllocateSpoutTask(Duration(restart.toInt, MILLISECONDS), "newLine")
  }
}
case class KafkaData(data:String)
class KafkaSpoutBackPressure(queue:LinkedBlockingQueue[String]) extends SpoutTrait {
  var startingSpeed     = System.getenv().getOrDefault("STARTING_SPEED", "1000").trim.toInt
  override protected def ProcessSpoutTask(receivedMessage: Any): Unit = receivedMessage match {
    case StartSpout => AllocateSpoutTask(Duration(1, MILLISECONDS), "newLine")
    case KafkaData(data) => queue.put(data)
    case "newLine"  => consumeFromQueue
    case _          => println("message not recognized!")
  }
  def consumeFromQueue() = {
    for(i<-0 to startingSpeed/100){
      if(!queue.isEmpty) {
        sendTuple(queue.take())
      }
    }
    AllocateSpoutTask(Duration(10, MILLISECONDS), "newLine")
  }
}
