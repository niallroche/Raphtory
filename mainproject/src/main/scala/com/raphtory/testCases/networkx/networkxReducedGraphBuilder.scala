package com.raphtory.testCases.networkx

import com.raphtory.core.actors.Router.GraphBuilder
import com.raphtory.core.model.communication._

class networkxReducedGraphBuilder extends GraphBuilder[String]{

  val preamble = 0
  val nodes = 1
  val edges = 2

//  var state = preamble

  override def parseTuple(record: String) = {
      var state = preamble
      var skip = false



      /*state match {
        case preamble => state = nodes; skip = true //skip first line
        case nodes => if ("\"links\":[".equals(record)) { state = edges ; skip = true }
        case edges => if ("]}".equals(record)) skip = true
      }*/

//      if(state == preamble) { state = nodes; skip = true }
      if (record.contains("{\"directed\": true")) { skip = true }
      if (record.contains("{\"id\":"))            { state = nodes}
      if ("\"links\":[".equals(record))           { skip = true }
      if (record.contains("{\"source\":"))        { state = edges}
      /*if(state == preamble) { state = nodes; skip = true }
      else if(state == nodes) { if ("\"links\":[".equals(record)) {
        state = edges ; skip = true } }
      else if(state == edges) { if ("]}".equals(record)) skip = true }*/

      if (!skip) {
        val properties = record.replaceAll("^\\{|\\}$", "").split(",")
          .map(_.split(":"))
          .map { case Array(key, value) => (key.trim() -> value.trim()) }.toMap
        val time = properties("\"t\"").toLong

        if (state == nodes) {
          val id = properties("{\"id\"").toLong
          sendUpdate(
            VertexAdd(
              //      VertexAddWithProperties(
              msgTime = time,
              srcID = id,
              //        Properties(StringProperty("Word", dp.head))
            )
          )
        } else if (state == edges) {
          val sourceId = properties("{\"source\"").toLong
          val targetId = properties("\"target\"").toLong
          sendUpdate(
            //      EdgeAddWithProperties(msgTime = time,
            EdgeAdd(msgTime = time,
              srcID = sourceId,
              dstID = targetId,
              //          Properties(LongProperty("weight", properties.getOrElse("'weight'", "1").toLong))
            )
          )
        }
      }

      /*val dp = record.split(";").map(_.trim)
      val properties = dp(2).replaceAll("^\\{|\\}$", "").split(",")
        .map(_.split(":"))
        .map { case Array(key, value) => (key.trim()-> value.trim()) }.toMap
      val srcClusterId = dp(0).toLong
      val dstClusterId = dp(1).toLong
      val time = properties("'t'").toLong


    sendUpdate(
      VertexAdd(
//      VertexAddWithProperties(
        msgTime = time,
        srcID = srcClusterId,
//        Properties(StringProperty("Word", dp.head))
      )
    )
    sendUpdate(
      VertexAdd(
//      VertexAddWithProperties(
        msgTime = time,
        srcID = dstClusterId,
//        Properties(StringProperty("Word", dp(1)))
      )
    )
    sendUpdate(
//      EdgeAddWithProperties(msgTime = time,
      EdgeAdd(msgTime = time,
          srcID = srcClusterId,
          dstID = dstClusterId,
//          Properties(LongProperty("weight", properties.getOrElse("'weight'", "1").toLong))
      )
    )
*/


    //    sendUpdate(
//        EdgeAddWithProperties(msgTime = time,
//          srcID = dstClusterId,
//          dstID = srcClusterId ,
//          Properties(LongProperty("weight", properties("'weight'").toLong))
//        )
//      )

    }
}
