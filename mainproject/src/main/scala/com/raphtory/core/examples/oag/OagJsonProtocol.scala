package com.raphtory.examples.oag

import java.net.URL

import akka.http.scaladsl.unmarshalling.Unmarshal
import com.raphtory.examples.oag
import com.raphtory.examples.oag.PublicationType
import com.raphtory.examples.oag.PublicationType.PublicationType
import com.raphtory.examples.oag.SourceUrlType.SourceUrlType
import spray.json.JsNumber

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
//import com.raphtory.examples.oag.PublicationType.PublicationType
import spray.json.{DefaultJsonProtocol, JsArray, JsObject, JsString, JsValue, RootJsonFormat}
;

object OAGJsonProtocol extends DefaultJsonProtocol {

//                implicit val gabAuthorFormat           = jsonFormat2(List[Author])
//implicit val publicationTypeFormat         = jsonFormat1(PublicationType.type)

  implicit val magExtendedReferenceFormat = jsonFormat2(MAGReference)

  implicit object OAGDocJsonFormat extends RootJsonFormat[OAGPaper] {
    // TODO Writer method

    def getRawField(field: String)(implicit jsObj: JsObject): Option[JsValue] =
      jsObj.getFields(field).headOption

    def getField(field: String)(implicit jsObj: JsObject): Option[String] =
      getRawField(field) match {
        case Some(s) => Some(s.toString())
        case None => None
      }

    def getPublicationTypeField(field: String)(implicit jsObj: JsObject): Option[PublicationType] =
      getField(field) match {
        case Some(s) => Option(parsePublicationType(s.replaceAll("\"", "").toInt))
        case None => None
      }

    def getSourceUrlTypeField(field: String)(implicit jsObj: JsObject): Option[SourceUrlType] =
      getField(field) match {
        case Some(s) => Option(parseSourceUrlType(s.replaceAll("\"", "").toInt))
        case None => None
      }

    def getDataSources(field: String)(implicit jsObj: JsObject): Option[List[DataSource]] = {
      return None
//    def getDataSources(field: String)(implicit jsObj: JsObject): Option[String] = {
      val sString = getRawField(field)
      if (sString == None) {
        return None
      }
      val sValue =  sString.get
      val sArray = sValue.asInstanceOf[JsArray]

//      var dataSources: List[DataSource] = List()
//      var dataSources = new ListBuffer[Option[DataSource]]()
      var dataSources = new ListBuffer[DataSource]()

      for (s <- sArray.elements) {
//        implicit val attributes = s
        /*getRawField(s) match {
          case Some(s) => Some(s.toString())
          case None => None
        }*/
//        s.
//        val dataSource = new DataSource(SourceUrlType.withName("HTML"), new URL("http://example.com"))


//        val st = SourceUrlType.apply(3)
//        val st = new Enumeration[SourceUrlType](3)


//        check the order of the fields
        var st = SourceUrlType.withName("UNKNOWN")
        var url = new URL("http://example.com")
        if (s.asInstanceOf[JsObject].fields.head._2.toString().forall(_.isDigit)) {
//          st = SourceUrlType.apply(s.asInstanceOf[JsObject].fields.head._2.toString().toInt)
          st = parseSourceUrlType(s.asInstanceOf[JsObject].fields.head._2.toString().toInt)
          url = new URL(s.asInstanceOf[JsObject].fields.last._2.toString().replaceAll("\"", ""))
        } else if (s.asInstanceOf[JsObject].fields.head._1.toString().equalsIgnoreCase("U")) {
//          st = SourceUrlType.apply(s.asInstanceOf[JsObject].fields.last._2.toString().toInt)
          st = SourceUrlType.UNKNOWN
          url = new URL(s.asInstanceOf[JsObject].fields.head._2.toString().replaceAll("\"", ""))
        } else {
          st = SourceUrlType.withName("UNKNOWN")
        }


        val dataSource = new DataSource(st, url)
//        val dataSource = None
//        dataSources:+dataSource
        dataSources+=dataSource
      }
//      implicit val attributes = sArray.elements(0).asJsObject

      return Some(dataSources.toList)
//      return None
    }

      /*getRawField(field) match {
        case Some(s) => Some(s.toString())
        case None => None
      }*/
      /*getField(field) match {
        case Some(s) => Option(parseSourceUrlType())
        case None => None
      }*/
//      l = List[DataSource]
      /*getRawField(field).toString match {
        case Some(f) => Some(f)
      }*/
      //      return new Option[List[DataSource]]
      //return Some(l)


      /*getRawField("field") match {
//        case Some(field) => Some(field.convertTo[parseDataSources])
        case Some(field) => Some(parseDataSources(field.asJsObject().getFields()))
        case None    => None
      }*/

    /*def parseDataSources(sources: String[]): List[DataSource] = {
      val sources = new List[DataSource]
      return sources
    }*/

    def getExtendedReferences(field: String)(implicit jsObj: JsObject): Option[List[MAGReference]] = {
      val refs = mutable.MutableList[MAGReference]()
      //"ExtendedRId": [{"RId": 2918032601, "title":"Paper1 title"}, {"RId": 2092553756, "title":"Paper2 title"}, {"RId": 1506366602, "title":"Paper2 title"}]
      val raw = getRawField(field)
      if (raw == None) {
        //        return new List[]
        return None
      }
      val sValue =  raw.get
      val sArray = sValue.asInstanceOf[JsArray]
//      val sArray_elements = sArray.elements
//      val r = Unmarshal(sValue).to[List[MAGReference]]
//      sArray_elements.foreach { elem =>
      for (r <- sArray.elements) {
//        val mag = elem.convertTo(MAGReference)
          val ref = r.asInstanceOf[JsObject]
          refs+=new MAGReference(ref.fields.get("RId").get.asInstanceOf[JsNumber].value.longValue(), ref.fields.get("title").get.toString)
        //
//        val mag = elem.asJsObject.fields.get("RId").get.asInstanceOf[Long].longValue()
      }
//      Some(jsObj.fields.get(field).asInstanceOf[List[MAGReference]])
//      Some(refs.toList)
//      val sArray = sValue.asInstanceOf[JsArray[MAGReference]]
//      val sArray = sValue.asInstanceOf[JsArray]
//      for(r <- sArray.elements) {
//      for(r <- sValue.elements) {
        //val ref = r.asInstanceOf[JsObject]
        //refs+=new MAGReference(ref.fields.get("RId").asInstanceOf[Long].longValue(), ref.fields.get("title").toString)
//        r.asInstanceOf[JsNumber].value.asInstanceOf[BigDecimal].longValue()
      //}
      Some(refs.toList)
    }

    def getReferences(field: String)(implicit jsObj: JsObject): Option[List[Long]] = {
      val refs = mutable.MutableList[Long]()
      val raw = getRawField(field)
      if (raw == None) {
//        return new List[]
        return None
      }
      val sValue =  raw.get
      val sArray = sValue.asInstanceOf[JsArray]
      for(r <- sArray.elements) {
        refs+=r.asInstanceOf[JsNumber].value.asInstanceOf[BigDecimal].longValue()
      }
      Some(refs.toList)
    }

    def getCitations(field: String)(implicit jsObj: JsObject): Option[List[Long]] = {
      val refs = mutable.MutableList[Long]()
      val raw = getRawField(field)
      val sValue =  raw.get
      val sJs = sValue.asInstanceOf[JsObject]
      for(c <- sJs.fields) {
//        refs+=c.asInstanceOf[JsNumber].value.asInstanceOf[BigDecimal].longValue()
        refs+=c._1.toLong
      }
      Some(refs.toList)
    }

//    def parsePublicationType(pubtype: Int): Option[PublicationType] =
    def parsePublicationType(pubtype: Int): PublicationType =
      pubtype match {
        case 0 => PublicationType.Unknown
        case 1 => PublicationType.JournalArticle
        case 2 => PublicationType.Patent
        case 3 => PublicationType.ConferencePaper
        case 4 => PublicationType.BookChapter
        case 5 => PublicationType.Book
        case 6 => PublicationType.BookReferenceEntry
        case 7 => PublicationType.Dataset
        case 8 => PublicationType.Repository
        case default => PublicationType.Unknown
      }

    def parseSourceUrlType(pubtype: Int): SourceUrlType =
      pubtype match {
        case 1 => SourceUrlType.HTML
        case 2 => SourceUrlType.TEXT
        case 3 => SourceUrlType.PDF
        case 4 => SourceUrlType.DOC
        case 5 => SourceUrlType.PPT
        case 6 => SourceUrlType.XLS
        case 7 => SourceUrlType.PS
        case default => SourceUrlType.UNKNOWN
      }


    def getBoolean(field: String)(implicit jsObj: JsObject): Option[Boolean] =
      getField(field) match {
        case Some(s) => Some(s.toBoolean)
        case None => None
      }

    def getInt(field: String)(implicit jsObj: JsObject): Option[Int] =
      getField(field) match {
        case Some(s) => Some(s.toInt)
        case None => None
      }

    def getLong(field: String)(implicit jsObj: JsObject): Option[Long] =
      getField(field) match {
        case Some(s) => Some(s.toLong)
        case None => None
      }

    def getDouble(field: String)(implicit jsObj: JsObject): Option[Double] =
      getField(field) match {
        case Some(s) => Some(s.toDouble)
        case None => None
      }

    def write(p: OAGPaper) = JsString("TODO")

    def read(value: JsValue) = {
        val jsObj = value.asJsObject
        val entitiesArray = jsObj.fields.get("entities")

        var raw_attributes = jsObj
        if (entitiesArray != None) {
          val jsonEntities =  entitiesArray.get
          val en = jsonEntities.asInstanceOf[JsArray]
          raw_attributes = en.elements(0).asJsObject
        }
//        implicit val attributes = en.elements(0).asJsObject
        implicit val attributes = raw_attributes

            new OAGPaper(
              getLong("Id"),
              getField("DOI") match {
                case Some(s) => Some(s.replaceAll("\"", ""))
                case None    => None
              },
              getLong("FamId"),
              getField("Ti") match {
                case Some(s) => Some(s.replaceAll("\\\\", "").replaceAll("\"", ""))
                case None    => None
              },
              getField("DN") match {
                case Some(s) => Some(s.replaceAll("\\\\", "").replaceAll("\"", ""))
                case None    => None
              },
//              getField("Pt"),
//              getSourceUrlTypeField("Ty"),
//              Option(SourceUrlType.withName("HTML")),
              getDataSources("S"),
              getPublicationTypeField("Pt"),
              getInt("Y"),
              getField("D") match {
                case Some(s) => Some(s.replaceAll("\"", ""))
                case None    => None
              },
              getInt("CC"),
              getInt("ECC"),
              getReferences("RId"),
              getExtendedReferences("ExtendedRId"),
//              getCitations("CitCon")
//              new Option(new List[String])
//              getBoolean("isSeed"),
              getField("s3_key") match {
                case Some(s) => Some(s.contains("/seed_papers/"))
                case None    => None
              },
              getField("s3_key") match {
                case Some(s) => Some(s.contains("/first_layer/"))
                case None    => None
              },
              getField("s3_key") match {
                case Some(s) => Some(s.contains("/second_layer/"))
                case None    => None
              },
              getField("s3_key") match {
                case Some(s) => Some(s.contains("/semanticscholar_label_density/"))
                case None    => None
              },
              getField("s3_key") match {
                case Some(s) => Some(s.contains("/mag_label_density/"))
                case None    => None
              },
              getField("label_density") match {
                case Some(s) => Some(s.toDouble)
                case None    => None
              },
              getField("s3_key") match {
                case Some(s) => Some(s)
                case None    => None
              },
              getDouble("num_blockchain_name"),
              getDouble("num_chargingandrewardingsystem"),
              getDouble("num_codebase"),
              getDouble("num_consensus"),
              getDouble("num_esg"),
              getDouble("num_extensibility"),
              getDouble("num_identifiers"),
              getDouble("num_identity_management"),
              getDouble("num_labelled_token"),
              getDouble("num_misc"),
              getDouble("num_native_currency_tokenisation"),
              getDouble("num_security_privacy"),
              getDouble("num_token"),
              getDouble("num_transaction_capabilities"),
            )
    }
  }
}