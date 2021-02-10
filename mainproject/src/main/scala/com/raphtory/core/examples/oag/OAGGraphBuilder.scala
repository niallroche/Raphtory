package com.raphtory.core.examples.oag
import java.time.{LocalDate, Month, ZonedDateTime}
import java.time.format.DateTimeFormatter
import java.time.temporal.ChronoUnit
import java.util

import com.raphtory.core.actors.Router.GraphBuilder
import com.raphtory.core.model.communication.{DoubleProperty, EdgeAdd, GraphUpdate, ImmutableProperty, LongProperty, Properties, Type, VertexAddWithProperties}
//import com.raphtory.core.model.communication
//import com.raphtory.core.model.communication.{StringSpoutGoing, _}
import com.raphtory.examples.oag.{OAGPaper, OpenAcademic}

import com.raphtory.examples.oag.OAGJsonProtocol._
//import com.raphtory.examples.oag.OpenAcademicJsonProtocol.OpenAcademicDocJsonFormat.{getBoolean, getField, getInt, getReferences}
import com.raphtory.examples.oag.OpenAcademicJsonProtocol._
import spray.json._

import scala.collection.mutable.ListBuffer
import scala.collection.parallel.mutable.ParHashSet

class OAGGraphBuilder extends GraphBuilder[String] {

//  var threshold = 2.2
  var filteringOnThreshold = System.getenv().getOrDefault("FILTERING_ON_THRESHOLD", "false").trim.toBoolean
//  var threshold = System.getenv().getOrDefault("THRESHOLD", "2.15").trim.toDouble
  var threshold = System.getenv().getOrDefault("THRESHOLD", ".0215").trim.toDouble

  private val processCitations = System.getenv().getOrDefault("PROCESS_CITATIONS", "false").trim.toBoolean

  /*override def parseTuple(tuple: String): Unit = {

    val fileLine = tuple.split("\t").map(_.trim)

    //create case
    sendUpdate(VertexAddWithProperties(
      dateToEarlierEpoch(fileLine(orig_date)), //orig_date
      fileLine(orig_vid).toInt, //orig_vid
      Properties(
        ImmutableProperty("orig_title", fileLine(orig_title)),
        ImmutableProperty("orig_id", fileLine(orig_id)),
        ImmutableProperty("Jurisdiction", fileLine(orig_country))
      ),
      Type("Case")
    ))
  }*/

  //override def parseTuple(tuple: StringSpoutGoing): ParHashSet[GraphUpdate] = {
  override def parseTuple(tuple: String): Unit = {
    val commands = new ParHashSet[GraphUpdate]()
    try {
      val command = tuple
      val inputType = resolveInputFormat(command)

      if (inputType == 0) { //sematic
        val doc = command.parseJson.convertTo[OpenAcademic]
        if (doc.s3_key != None) {
          println("s3_key" + doc.s3_key)
          println("doc.isSeed" +doc.isSeed)
        }
        sendDocumentToPartitions(doc, commands)
      } else if (inputType == 1) { //MAG
        val oagdoc = command.parseJson.convertTo[OAGPaper]
        if (oagdoc.s3_key != None) {
          println("s3_key" + oagdoc.s3_key)
          println("doc.isSeed" +oagdoc.isSeed)
        }
        val doc = convertToOpenAcademic(oagdoc)
        sendDocumentToPartitions(doc, commands)
      } else {
        println("Could not resolve input type")
      }
    } catch {
      case e: Exception => println("Could not parse input" + e.getMessage)
    }
  }

  def resolveInputFormat(inputstr: String): Int = {

    if (inputstr.contains("num_blockchain_name")) {
      println("newdata")
      if (!inputstr.contains("num_blockchain_name\": -1,")) {
        println("newdata with value")
      }
    }

    if (inputstr.contains("168635309") && inputstr.contains("352507459")) {
      println("found paper")
    }

    if (inputstr.contains("2126031951") && inputstr.contains("212119943")) {
//    if (inputstr.contains("a survey on distributed topology control techniques for extending the lifetime of battery powered wireless sensor networks")) {
//    if (inputstr.contains("intelligent device to device communication in the internet of things")) {
      println("found paper")
    }

      //    if (inputstr.contains("entities") && inputstr.contains("FamId"))//is MAG format
//    if (inputstr.contains("FamId"))//is MAG format
    if (inputstr.contains("Ti") && inputstr.contains("Ty") && inputstr.contains("Id")
      && inputstr.contains("logprob")
      )//is MAG format
      return 1
    else if (inputstr.contains("arxivId")) {
      return 0 //other like Semantic Scholar
    }
    return -1
  }

  def convertToOpenAcademic(paper:OAGPaper) :OpenAcademic = {
//    var references : List[OpenAcademic] = List()
    var references = new ListBuffer[OpenAcademic]
    for (reference <- paper.extendedReferences.get) {
//    paper.extendedReferences.get.foreach { reference =>
//    paper.extendedReferences.get.foreach { reference =>
//      val r = reference.get
      val openformat = new OpenAcademic(
        Some(reference.title),
        None,
        Some(reference.RId.toString),
        None,
        None,
        None,
        None,
        None,
        None,
        None,
        None,
        None,
        None,
        None,
        None,
        None,
        None,
        None,
        None,
        None,
        None,
        None,
        None,
        /*reference.num_blockchain_name,
        reference.num_chargingandrewardingsystem,
        reference.num_codebase"),
        reference.num_consensus"),
        reference.num_esg"),
        reference.num_extensibility"),
        reference.num_identifiers"),
        reference.num_identity_management"),
        reference.num_labelled_token"),
        reference.num_misc"),
        reference.num_native_currency_tokenisation"),
        reference.num_security_privacy"),
        reference.num_token"),
        reference.num_transaction_capabilities"),*/
      )
      references+=openformat
    }

//    var references = None
    var citations = None

    new OpenAcademic(
      paper.title,
      paper.doi,
      Some(paper.id.toString),
      paper.year,
      Some(references.toList),
      citations,
      paper.isSeed,
      paper.labelDensity,
      paper.s3_key,
      paper.num_blockchain_name,
      paper.num_chargingandrewardingsystem,
      paper.num_codebase,
      paper.num_consensus,
      paper.num_esg,
      paper.num_extensibility,
      paper.num_identifiers,
      paper.num_identity_management,
      paper.num_labelled_token,
      paper.num_misc,
      paper.num_native_currency_tokenisation,
      paper.num_security_privacy,
      paper.num_token,
      paper.num_transaction_capabilities
    )
  }

  def sendDocumentToPartitions(document: OpenAcademic, commands: ParHashSet[GraphUpdate]): Unit = {
//    if (checkThreshold(document)) return
    if (checkThreshold(document) && (document.isSeed == None || document.isSeed.get.booleanValue() == false)) return
    //    var timestamp = dateToUnixTime(document.date.get.toString)
    //    var timestamp = dateToEarlierEpoch(document.date.get.toString)
    var timestamp = dateToEarlierEpoch(document.year.get.toString)
    //    var annotationUUID = mapId(document.id.get)
        var annotationUUID = assignID(document.title.get)
//    var annotationUUID = assignPaperID(document.paperId.get)
    //send paper and add edges to references and back to citations
    //    val props = new Properties()
    //    props+=new StringProperty("doi",document.doi.get)

    document.title.get
    //    if (document.title.get.contains("turtle")) {
    /*if (document.title.get.contains("turtle")) {
      print("-----found turtles paper")
    }
    if (document.title.get.contains("Blockchain for Cities")) {
      print("-----found Blockchain for Cities paper")
    }
    if (document.title.get.contains("The tragedy of the commons.")) {
      print("-----found Tragedy of the Commons paper")
    }*/

    sendUpdate(
      VertexAddWithProperties(
        timestamp,
        annotationUUID,
        Properties(
          //          StringProperty("doi",document.doi.get),
          if (document.doi != None)
            ImmutableProperty("doi", document.doi.get)
          else
            ImmutableProperty("doi",""),
          ImmutableProperty("title",document.title.get),
          LongProperty("year",document.year.get.toLong),
          //ImmutableProperty("date",document.date.get.toString),
          if (document.isSeed != None && document.isSeed.get)
            LongProperty("isSeed", 1)
          else
            LongProperty("isSeed", 0),
          //LongProperty("isSeed", 1)//hardocding seed for the moment
          if (document.labelDensity != None)
            DoubleProperty("label_density", document.labelDensity.get)
          else
            DoubleProperty("no_label_density", 1),
//            null,
          LongProperty("loadedDirectly", 1),

          ImmutableProperty("s3_key",if (document.s3_key == None) "None" else document.s3_key.get),
          DoubleProperty("num_blockchain_name",if (document.num_blockchain_name == None) -1 else document.num_blockchain_name.get),
          DoubleProperty("num_chargingandrewardingsystem",if (document.num_chargingandrewardingsystem == None) -1 else document.num_chargingandrewardingsystem.get),
          DoubleProperty("num_codebase",if (document.num_codebase == None) -1 else document.num_codebase.get),
          DoubleProperty("num_esg",if (document.num_esg == None) -1 else document.num_esg.get),
          DoubleProperty("num_extensibility",if (document.num_extensibility == None) -1 else document.num_extensibility.get),
          DoubleProperty("num_identifiers",if (document.num_identifiers == None) -1 else document.num_identifiers.get),
          DoubleProperty("num_identity_management",if (document.num_identity_management == None) -1 else document.num_identity_management.get),
          DoubleProperty("num_labelled_token",if (document.num_labelled_token == None) -1 else document.num_labelled_token.get),
          DoubleProperty("num_misc",if (document.num_misc == None) -1 else document.num_misc.get),
          DoubleProperty("num_native_currency_tokenisation",if (document.num_native_currency_tokenisation == None) -1 else document.num_native_currency_tokenisation.get),
          DoubleProperty("num_security_privacy",if (document.num_security_privacy == None) -1 else document.num_security_privacy.get),
          DoubleProperty("num_token",if (document.num_token == None) -1 else document.num_token.get),
          DoubleProperty("num_transaction_capabilities",if (document.num_transaction_capabilities == None) -1 else document.num_transaction_capabilities.get),
        ),
        Type("Paper"),
      )
      )

    //    add outbound references
    if (document.references != None) {
      handleReferences(document.references.get, commands, timestamp, annotationUUID, true)
    }

    //    citation handling is configured externally, an extension would be to make this configurable at a layer level
    if (processCitations) {
      //    add inbound citations
      if (document.citations != None) {
        handleReferences(document.citations.get, commands, timestamp, annotationUUID, false)
      }
    }
    /*for(citation <- document.citations.get) {
      val refUUID = mapId(citation)
      commands+=(
        EdgeAdd(
          timestamp,
          annotationUUID,
          refUUID,
          Type("cites_document")
        )
      )
    }*/

    //    println("added timestamp:" + timestamp)
//    commands
  }

  private def checkThreshold(document: OpenAcademic): Boolean = {
    if (filteringOnThreshold) {
      val density = document.labelDensity
      if (density isDefined) {
        if (document.labelDensity.get.doubleValue() == 0)
          println("0")
        if (document.labelDensity.get.doubleValue() != -1) {
          if (document.labelDensity.get.doubleValue < threshold.doubleValue || document.labelDensity.get.doubleValue > 100)
            return true //do not proceed with adding this node
          else
            println("over")
        } else {
          println("-1")
          return true
        }
      } else {
        println("not defined")
        return true
      }
    }
    false
  }

  private def handleReferences(references: List[OpenAcademic], commands: ParHashSet[GraphUpdate], timestamp: Long, annotationUUID: Long, outbound: Boolean) = {
    for (reference <- references) {
      //        val refUUID = mapId(reference)
      //        val refUUID = assignID(reference.title.get)
//      val refUUID = assignPaperID(reference.paperId.get)
      val refUUID = assignPaperID(reference.title.get)
      //      if (reference.title.get.contains("Blockchain for Cities A Systematic Literature Review")) {
      if (reference.title.get.contains("Blockchain for Cities A Systematic Literature Review")) {
        print("-----found ref to Blockchain for Cities paper")
      }
      if (reference.title.get.contains("The tragedy of the commons.")) {
        print("-----found Tragedy of the Commons paper")
      }

      var addToGraph = false //default to just adding edge as the data will be loaded in later/already
      /*var addToGraph = true
      if (reference.year == None)
        addToGraph = false
      if(checkThreshold(reference))
        addToGraph = false*/


      if (addToGraph) sendUpdate(
        VertexAddWithProperties(
          dateToEarlierEpoch(reference.year.get.toString),
          refUUID,
          Properties(
            //          StringProperty("doi",document.doi.get),
            if (reference.doi != None)
              ImmutableProperty("doi", reference.doi.get)
            else
              ImmutableProperty("doi", ""),
            ImmutableProperty("title", reference.title.get),
            LongProperty("year", reference.year.get.toLong),
            //ImmutableProperty("date",document.date.get.toString),
            if (reference.isSeed != None && reference.isSeed.get)
              LongProperty("isSeed", 1)
            else
              LongProperty("isSeed", 0),
//            DoubleProperty("labelDensity", reference.labelDensity.get),
            //            LongProperty("isSeed", 0),
            LongProperty("loadedViaReference", 1),
            if (reference.labelDensity != None)
              DoubleProperty("label_density", reference.labelDensity.get)
            else
              DoubleProperty("no_label_density", 1)
            /*if (reference.labelDensity != None) {
              val label_density = reference.labelDensity.get
              if (label_density > threshold)
                DoubleProperty("labelDensity", reference.labelDensity.get)
              else {
                print("skipping as label density:" + label_density + " is not greater than threshold:" + threshold)
                null
              }
            } else
//              DoubleProperty("labelDensity", 0)
              null*/

          ),
          Type("Paper"),
        )
      )

      //        val refUUID = assignID(reference.title.get)
      sendUpdate(
        EdgeAdd(
          timestamp,
          if (outbound) annotationUUID else refUUID,//source paper
          if (outbound) refUUID else annotationUUID,//destination paper
          Type("document_references")
        )
      )
    }
  }

  def assignPaperID(paperId: String) :Long = {
    /*if ("bc122501e2ad6292d3dcaf29a68aff9025b72e53" == paperId) {
      print("-----found paper")
    }*/
    assignID(paperId)
  }

  def dateToEarlierEpoch(timestamp: => String): Long = {
    if (timestamp == "-1")
      return -1
    val startEpoch = LocalDate.of(1600, Month.JANUARY, 1)

    //convert String to LocalDate for comparison
    //need to check if the field contains a full date, otherwise default to the start of that year
    var compareDate = startEpoch
    var formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd")
    if ("Invalid date".equals(timestamp)) {
      compareDate = LocalDate.now()
    } else if (timestamp.indexOf('-') == -1) {
      compareDate = LocalDate.of(Integer.parseInt(timestamp), Month.JANUARY, 1)
    } else if (timestamp.indexOf('T') != -1) {
      //2010-03-17T00:00:00.000Z examples like these
      //      formatter = DateTimeFormatter.ISO_INSTANT
      //      compareDate = LocalDate.parse(timestamp, formatter)
      val zone_dt = ZonedDateTime.parse(timestamp)
      compareDate = zone_dt.toLocalDate()
    } else if ("Invalid date".equals(timestamp)) {
      compareDate = LocalDate.now()
    }else compareDate = LocalDate.parse(timestamp, formatter)

    //    val gap = ChronoUnit.MILLIS.between(startEpoch, compareDate);
    //    val gap = ChronoUnit.SECONDS.between(startEpoch, compareDate);
    val gap = ChronoUnit.DAYS.between(startEpoch, compareDate);
    gap
  }
}