package querki.core

import scala.xml.Elem

import models.{DisplayPropVal, OID, Property, PTypeBuilder, SimplePTypeBuilder, Wikitext}
import models.Thing.PropFetcher

import models.system.{SystemType, CommonInputRenderers}

import querki.ecology._

import ql.{QLParser, QLPhrase}
import querki.util.PublicException
import querki.values.{ElemValue, ParsedTextType, QLContext, QValue, SpaceState}

import MOIDs._
  
/**
 * Trivial marker trait, that simply identifies the "Text Types" that are similarly serializable.
 */
trait IsTextType

trait TextTypeBasis { self:CoreEcot =>
  
  trait TextTypeUtils { self:SystemType[_] =>
    def validateText(v:String, prop:Property[_,_], state:SpaceState):Unit = {
      for (
        minLengthVal <- prop.getPropOpt(Types.MinTextLengthProp)(state);
        minLength <- minLengthVal.firstOpt
        if (v.trim().length() < minLength)
          )
        throw new PublicException("Types.Text.tooShort", prop.displayName, minLength)
    }  
  }
  
  abstract class TextTypeBase(oid:OID, pf:PropFetcher) extends SystemType[QLText](oid, pf
      ) with PTypeBuilder[QLText,String] with querki.ql.CodeType with IsTextType with TextTypeUtils
  {
    def doDeserialize(v:String) = QLText(v)
    def doSerialize(v:QLText) = v.text
    def doWikify(context:QLContext)(v:QLText, displayOpt:Option[Wikitext] = None) = {
      val parser = new QLParser(v, context)
      parser.process
    }
    val doDefault = QLText("")
    def wrap(raw:String):valType = QLText(raw)
    
    override def validate(v:String, prop:Property[_,_], state:SpaceState):Unit = validateText(v, prop, state)

    // TBD: in principle, we really want this to return a *context*, not a *value*. This is a special
    // case of a growing concern: that we could be losing information by returning QValue from
    // qlApply, and should actually be returning a full successor Context.
    // TODO: merge this with the fairly-similar code in QLType
    override def qlApplyFromProp(definingContext:QLContext, incomingContext:QLContext, prop:Property[QLText,_], params:Option[Seq[QLPhrase]]):Option[QValue] = {
      // TBD: in a perfect world, this would be a true Warning: legal, but with a warning on the side. Unfortunately, doing it
      // like this gets in the way of perfectly legit ordinary situations, and violates the general Querki data model, that
      // passing None to a Stage usually results in None. (That is, it violates ordinary map semantics.)
//      if (definingContext.isEmpty) {
//        Some(WarningValue("""Trying to use Text Property """" + prop.displayName + """" in an empty context.
//This often means that you've invoked it recursively without saying which Thing it is defined in."""))
//      } else {
        Some(incomingContext.collect(ParsedTextType) { elemContext:QLContext =>
          prop.applyToIncomingThing(definingContext) { (thing, context) =>
            implicit val s = definingContext.state
            // In other words, map over all the text values in this property, parsing all of them
            // and passing the resulting collection along the pipeline:
            thing.map(prop, ParsedTextType) { qlText =>
              val parser = new QLParser(qlText, elemContext, params)
              parser.process
            }
          }
        })
//      }
    }
      
    def code(elem:ElemValue):String = get(elem).text
  }

}

private[core] trait TypeCreation { self:CoreEcot with TextTypeBasis =>
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

  /**
   * The Type for Text -- probably the most common type in Querki
   */
  class TextType extends TextTypeBase(TextTypeOID,
      toProps(
        setName("Text Type")
        )) with PTypeBuilder[QLText,String] 
  {
    override def editorSpan(prop:Property[_,_]):Int = 12    
  }

  /**
   * The Type for Large Text -- stuff that we expect to take up more space on-screen
   */
  class LargeTextType extends TextTypeBase(LargeTextTypeOID,
      toProps(
        setName("Large Text Type")
        )) with PTypeBuilder[QLText,String] 
  {
    override def editorSpan(prop:Property[_,_]):Int = 12
    
    override def renderInputXml(prop:Property[_,_], state:SpaceState, currentValue:DisplayPropVal, v:ElemValue):Elem =
      renderLargeText(prop, state, currentValue, v, this)
  }
}