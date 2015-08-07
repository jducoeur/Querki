package querki.imexport

import fastparse.all._

/**
 * Given some XML input, this does the first-level parse into raw syntactic constructs.
 * 
 * As of this writing, this is very first-draft, and makes no promises about being able to 
 * parse arbitrary XML.
 * 
 * Really, we're doing this mostly because I wanted to learn FastParse. If it proves
 * insufficient, we may just switch to using scala.xml instead.
 */
object XMLParser {
  case class XmlName(ns:Option[String], name:String)
  case class XmlAttr(name:XmlName, v:String)
  sealed trait XmlNode
  case class XmlText(s:String) extends XmlNode
  case class XmlElement(tagName:XmlName, attrs:Seq[XmlAttr], children:Seq[XmlNode]) extends XmlNode {
    def attrOpt(name:String) = attrs.find(_.name.name == name)
    def attr(name:String) = attrOpt(name).get
    
    def childOpt(name:String):Option[XmlElement] = children.find {
      case XmlElement(cname, _, _) if (cname.name == name) => true
      case _ => false
    }.map(_.asInstanceOf[XmlElement])
    def child(name:String) = childOpt(name).getOrElse(throw new Exception(s"Failed to find node $name under $tagName"))
    def childrenNamed(name:String):Seq[XmlElement] = {
      children.map {
        case elem @ XmlElement(cname, _, _) if (cname.name == name) => Some(elem.asInstanceOf[XmlElement])
        case _ => None
      }.flatten
    }
    def elements:Seq[XmlElement] = {
      children.map {
        case elem @ XmlElement(_, _, _) => Some(elem.asInstanceOf[XmlElement])
        case _ => None
      }.flatten
    }
    def textOpt:Option[String] = {
      children.find {
        case XmlText(s) => true
        case _ => false
      }.map(_.asInstanceOf[XmlText].s)
    }
    
    def checkIs(name:String) = if (tagName.name != name) throw new Exception(s"Was expecting tag $name, found $tagName") 
  }
  
  
  /**
   * Some common entities that tend to show up in named form.
   * 
   * TODO: we should flesh out this list.
   */
  val xmlEntities = Map[String, Char](
    ("gt" -> '>'),
    ("lt" -> '<'),
      
    ("apos" -> '\''),
    ("quot" -> '"'),
    ("amp" -> '&'),
    ("tilde" -> '~'),
    
    // Fancy quotes, somewhat common in cut-and-paste:
    ("ldquo" -> 147),
    ("rdquo" -> 148),
    ("bdquo" -> 132),
    ("lsquo" -> 145),
    ("rsquo" -> 146),
    ("sbquo" -> 130),
      
    ("trade" -> 153),
    ("reg" -> 174),
    ("copy" -> 169)
  )
  
  val whitechar = P(CharIn(" \r\n\t"))
  val white = P(whitechar.rep(1))
  val optwhite = P(whitechar.rep)
  
  val xmlNameStartP = P(CharPred(c => CharPredicates.isLetter(c) || c == '_').!)
  val xmlNameCharP = P(CharPred(c => CharPredicates.isLetter(c) || CharPredicates.isDigit(c) || c == '_' || c == '-' || c == '.').!)
  val xmlNameP = P(((xmlNameStartP ~ xmlNameCharP.rep).! ~ ":").? ~ (xmlNameStartP ~ xmlNameCharP.rep).!).map { strs =>
    val (nsopt, name) = strs
    XmlName(nsopt, name)
  }
  
  /**
   * Parses a single Entity by name (eg, "&quot;"), and returns the character.
   */
  val xmlEntityNameP = P(StringIn(xmlEntities.keys.toSeq:_*).!).map(xmlEntities(_))
  val xmlEntityNumP = P("#" ~ (CharIn('0' to '9')).rep(1).!).map(Integer.parseInt(_)).map(_.toChar)
  val xmlEntityP = P("&" ~! (xmlEntityNameP | xmlEntityNumP) ~ ";")
  val char:Parser[Char] = P(AnyChar.!).map(_.head)
  val xmlTextChar:Parser[Char] = P((xmlEntityP | char))
  val xmlTextP = P((!"<" ~ xmlTextChar).rep(1)).map(chars => XmlText(chars.mkString))
  
  val xmlAttrP = P(xmlNameP ~ "=" ~ "\"" ~ (!"\"" ~ AnyChar).rep.! ~ "\"").map { strs =>
    val (name, v) = strs
    XmlAttr(name, v)
  }
  val xmlHeadP = P("<" ~ xmlNameP ~ optwhite ~ xmlAttrP.rep(sep=white ~ Pass)).map { elems =>
    val (name, attrs) = elems
    XmlElement(name, attrs, Seq.empty)
  }
  val emptyXmlP = P(xmlHeadP ~ "/>")
  // NOTE: this clumsily throws an exception if there is a name mismatch, mainly because I'm concerned
  // about the performance implications of flatMap. This needs more examination:
  val xmlWithChildrenP = P(xmlHeadP ~ ">" ~! (xmlTextP | xmlElementP).rep ~ "</" ~ xmlNameP ~ ">").map { elems =>
    val (head, children, tail) = elems
    if (head.tagName != tail)
      throw new Exception(s"Mismatched tags in XML: expecting ${head.tagName}, got $tail")
    head.copy(children = children)
  }
  val xmlElementP:Parser[XmlElement] = P(emptyXmlP | xmlWithChildrenP)
  
  val xmlPreludeP = "<?xml" ~ optwhite ~ 
    ("version=\"" ~ (!"\"" ~ AnyChar).rep ~ "\"").? ~ optwhite ~
    ("encoding=\"" ~ (!"\"" ~ AnyChar).rep ~ "\"").? ~
    "?>"
    
  /**
   * The main entry point. Feed this a complete XML file.
   */
  val xmlP = P(xmlPreludeP.? ~ xmlElementP)
}
