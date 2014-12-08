package ScalaBrackets.Utility

import org.json4s.CustomSerializer
import org.json4s.JsonAST.JNull

//http://stackoverflow.com/a/17870196/1469797 -- thanks theon!
class NoneJNullSerializer extends CustomSerializer[Option[_]](format => ({ case JNull => None }, { case None => JNull }))
