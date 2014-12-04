package ScalaBrackets

import org.json4s.JsonAST.JObject

/**
 * Created by Matthew on 11/24/2014.
 */
case class Participant(id: Int, payload: Option[JObject] = None)
