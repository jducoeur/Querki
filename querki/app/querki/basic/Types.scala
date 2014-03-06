package querki.basic

import querki.ecology._

import models.{OID, Property, PTypeBuilder, Wikitext}

import querki.core.{IsTextType, NameableType, TextTypeBasis}

import querki.values.{ElemValue, QLContext, SpaceState}

import MOIDs._

trait PlainTextBaseType { self:QuerkiEcot with TextTypeBasis =>
  abstract class PlainTextType(tid:OID, pf:PropFetcher) extends SystemType[PlainText](tid, pf) 
    with PTypeBuilder[PlainText,String] with IsTextType with NameableType with TextTypeUtils
  {
    def doDeserialize(v:String)(implicit state:SpaceState) = PlainText(v)
    def doSerialize(v:PlainText)(implicit state:SpaceState) = v.text
  
    def getName(context:QLContext)(v:ElemValue):String = get(v).text
  
    def doWikify(context:QLContext)(v:PlainText, displayOpt:Option[Wikitext] = None) = Wikitext(v.text)
  
    override def validate(v:String, prop:Property[_,_], state:SpaceState):Unit = validateText(v, prop, state)
  
    def doDefault(implicit state:SpaceState) = PlainText("")
    override def wrap(raw:String):valType = PlainText(raw)
  }
}
