/*
*  Json Library with one purppose, and that purpose is the creation of json from a few scala data types.
*  Supported collection types: Map[String, Any], List[Any] where Any is actually String, Number, Boolean
*
*  Example
*    Map("name" -> "Chas", "age" -> 25).toJson //= {"name":"Chas","age":25}
*    SortedMap("experience" -> "None, "name" -> "Chas", "age" -> 25).toJson //= {""experience":"None",name":"Chas","age":25}
*    List(1, 2, 3, "no", false).toJson //= [1,2,3,"no",false]
*  
*  Notes:
*    Both types (Map, List) can be nested json types
*
*  TODO:
*    Write tests
*    Remove JsonField Class, because we do not want someone to do ("name", "Chas").toJson (does not create valid json)
*/

package json

object Json {
  class JsonNode(val value: Any) {
    def toJson:String = {
      value match {
        case (v: String)              => String.format(""""%s"""", v.replaceAll("\"", "\\\\\""))
        case (v: Number)              => String.format("""%s""",   v.toString)
        case (v: Boolean)             => String.format("""%s""",   v.toString)
        case Some(v: JsonNode)        => v.toJson
        case (v: Tuple2[String, Any]) => v.toJson
        case (v: Map[String, Any])    => v.toJson
        case (v: List[Any])           => v.toJson
        case (x: AnyRef)              => x.getClass.toString
        case (null)                   => ""
      }
    }
  }

  implicit def ListToJsonList(list:List[Any]):JsonList = new JsonList(list.map(new JsonNode(_)))
  implicit def Tuple2ToJsonField(tup:Tuple2[String, Any]):JsonField = new JsonField(tup._1, new JsonNode(tup._2))
  implicit def MapToJsonObject(obj:Map[String, Any]):JsonObject = new JsonObject(obj.map(x => (x._1, Some(new JsonNode(x._2)))))

  class JsonField(tup:Tuple2[String, JsonNode]) extends JsonNode {
    override def toJson:String = String.format(""""%s":%s""", tup._1, tup._2.toJson)
  }

  class JsonObject(obj:Map[String, Option[JsonNode]]) extends JsonNode {
    override def toJson:String = String.format("""{%s}""", obj.filterNot(_._2.get.value == None).map(_.toJson).mkString(","))
  }

  class JsonList(list:List[JsonNode]) extends JsonNode {
    override def toJson:String = String.format("""[%s]""", list.filterNot(_.value == None).map(_.toJson).mkString(","))
  }
}