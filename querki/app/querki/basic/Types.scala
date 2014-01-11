package querki.basic

import querki.ecology._

import models.{OID, Property, PTypeBuilder, Wikitext}

import models.system.{SystemType, NameableType}

import querki.core.{IsTextType, TextTypeBasis}

import querki.values.{ElemValue, QLContext, SpaceState}

import MOIDs._

trait PlainTextBaseType { self:QuerkiEcot with TextTypeBasis =>
  abstract class PlainTextType(tid:OID, actualName:String) extends SystemType[PlainText](tid,
    toProps(
      setName(actualName)
    )) with PTypeBuilder[PlainText,String] with IsTextType with NameableType with TextTypeUtils
  {
    def doDeserialize(v:String) = PlainText(v)
    def doSerialize(v:PlainText) = v.text
  
    def getName(context:QLContext)(v:ElemValue):String = get(v).text
  
    def doWikify(context:QLContext)(v:PlainText, displayOpt:Option[Wikitext] = None) = Wikitext(v.text)
  
    override def validate(v:String, prop:Property[_,_], state:SpaceState):Unit = validateText(v, prop, state)
  
    val doDefault = PlainText("")
    override def wrap(raw:String):valType = PlainText(raw)
  }
}
