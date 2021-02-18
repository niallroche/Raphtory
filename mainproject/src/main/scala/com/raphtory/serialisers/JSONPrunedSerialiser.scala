package com.raphtory.serialisers

import com.raphtory.api.Serialiser
import com.raphtory.core.model.analysis.entityVisitors.{EdgeVisitor, VertexVisitor}

import scala.collection.parallel.mutable.ParTrieMap

class JSONPrunedSerialiser extends Serialiser {

  override def startOfFile(): String = "{\"directed\": true, \"multigraph\": false, \"graph\": {}, \"nodes\": [\n"

  override def middleOfFile() :String = "],\n\"links\":[\n"

  override def fileExtension(): String = {
    "json"
  }

  override def serialiseVertex(v: VertexVisitor): String = {
    val properties: String = extractProperties(v.getPropertySet())

    if(properties.nonEmpty) {
      if (v.Type().nonEmpty)
        s"""\t{\"id\":${v.ID()},\"t\":${v.latestActivity()._1},\"doctype\":\"${v.Type()}\",$properties}"""
      else
        s"""\t{\"id\":${v.ID()},\"t\":${v.latestActivity()._1},$properties}"""
    }
    else {
      if (v.Type().nonEmpty)
        s"""\t{\"id\":${v.ID()},\"t\":${v.latestActivity()._1},\"doctype\":\"${v.Type()}\"}"""
      else
//        s"""\t{\"id\":${v.ID()}}"""
        s""
    }
  }

  private def extractProperties(ps: ParTrieMap[String,Any]) = {
    ps.map(property =>
      if (property._2.isInstanceOf[Long])
        s"""\"${property._1}\":${property._2}"""
      else
        s"""\"${property._1}\":\"${property._2}\""""
    ).toArray.mkString(",")
  }

  override def serialiseEdge(e: EdgeVisitor): String = {
    val properties = extractProperties(e.getPropertySet())
    if(properties.nonEmpty) {
      if (e.Type().nonEmpty)
        s"""\t{\"source\":${e.src()},\"target\":${e.dst()},\"t\":${e.latestActivity()._1},\"edgetype\":\"${e.Type()}\",$properties}"""
      else
        s"""\t{\"source\":${e.src()},\"target\":${e.dst()},\"t\":${e.latestActivity()._1},$properties}"""
    }
    else {
      if (e.Type().nonEmpty)
        s"""\t{\"source\":${e.src()},\"target\":${e.dst()},\"t\":${e.latestActivity()._1},\"edgetype\":\"${e.Type()}\"}"""
      else
        s"""\t{\"source\":${e.src()},\"target\":${e.dst()},\"t\":${e.latestActivity()._1}}"""
    }
  }

  override def endOfFile(): String = "\n]}\n"

}
