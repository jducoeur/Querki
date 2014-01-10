package querki.core

import models.{PTypeBuilder, SimplePTypeBuilder, Wikitext}

import models.system.SystemType

import querki.ecology._

import querki.values.{QLContext}

import MOIDs._

private[core] trait TypeCreation { self:CoreEcot =>
  class InternalMethodType extends SystemType[String](InternalMethodOID,
    toProps(
      setName("Internal Method Type")//,
//      Core.InternalProp(true)
    )) with SimplePTypeBuilder[String]
  {
    def boom = throw new Exception("InternalMethodType cannot be used conventionally. It simply wraps code.")
    def doDeserialize(v:String) = boom
    def doSerialize(v:String) = boom

    def doWikify(context:QLContext)(v:String, displayOpt:Option[Wikitext] = None) = Wikitext("Internal Method")
    
    val doDefault = ""
    override def wrap(raw:String):valType = boom 
  }
}