package com.raphtory.core.analysis.Algorithms

import com.raphtory.core.analysis.API.Analyser
import com.raphtory.core.utils.Utils

import scala.collection.mutable.ArrayBuffer
import scala.collection.parallel.immutable

class TemporalTriangleCount(args:Array[String]) extends Analyser(args) {

  override def setup(): Unit =
    proxy.getVerticesSet().foreach { v =>
      val vertex = proxy.getVertex(v._2)
      val t_max = vertex.getIncEdges.map(edge => edge._2.previousState.maxBy(f=> f._1)).max._1 //get incoming edges and then find the most recent edge with respect to timestamp and window
      vertex.getOutgoingNeighborsBefore(t_max).foreach(neighbour => {
        val nID = neighbour._1
        vertex.messageNeighbour(nID,(Array(v._1),t_max))
      })
    }

  override def analyse(): Unit =
    proxy.getVerticesWithMessages().foreach { v =>
      val vertex = proxy.getVertex(v._2)
      val queue = vertex.messageQueue.map(_.asInstanceOf[Tuple2[Array[Long],Long]])
      queue.foreach(message=> {
        val path = message._1
        val sender = path(path.length-1)
        val t_max = message._2
        val t_min = vertex.getIncEdges.get(sender).get.previousState.minBy(state => state._1)._1 //to include deletions check
        if(path.length<2) { //for step two of the algorithm i.e. the second node in the triangle
          vertex.getOutgoingNeighborsBetween(t_min, t_max).foreach(neighbour => {
            val nID = neighbour._1
            vertex.messageNeighbour(nID, (message._1 ++ Array(v._1), t_max))
          })
        }
        else{ //for the 3rd node in the triangle to see if the final edge exists
          val source = path(0)
          vertex.getOutgoingNeighborsBetween(t_min,t_max).get(source) match {
            case Some(edge) => vertex.appendToCompValue("TrianglePath",path ++ Array(v._1).toString)
            case None => //No triangle for you
          }
        }
      })
      vertex.clearQueue
    }

  override def returnResults(): Any =
    proxy.getVerticesSet().flatMap(v =>{
      val vertex = proxy.getVertex(v._2)
      vertex.getCompValue("TrianglePath") match {
        case Some(value) => value.asInstanceOf[Array[String]]
        case None => ""
      }//turn into Option
    })


  override def processResults(results: ArrayBuffer[Any], timeStamp: Long, viewCompleteTime: Long): Unit = {
    val endResults = results.asInstanceOf[ArrayBuffer[Array[String]]].flatten
    var toPublish = s"""{"timestamp":$timeStamp,triangles:["""
   // endResults.foreach(triangle => toPublish+= triangle+=",")
   // toPublish.dropRight(1)+="]}"
    publishData(toPublish)
  }

  override def defineMaxSteps(): Int = 2
}