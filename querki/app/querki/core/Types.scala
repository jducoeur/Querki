package querki.core

import scala.xml.Elem

import models.{DisplayPropVal, OID, Property, PTypeBuilder, PTypeBuilderBase, SimplePTypeBuilder, Thing, UnknownOID, Wikitext}
import models.Thing.PropFetcher

import models.system.{SystemType, CommonInputRenderers, NameableType, NameType}

import querki.ecology._

import ql.{QLParser, QLPhrase}
import querki.util.PublicException
import querki.values.{ElemValue, ParsedTextType, QLContext, QValue, SpaceState}

import MOIDs._
  
/**
 * Trivial marker trait, that simply identifies the "Text Types" that are similarly serializable.
 */
trait IsTextType
  
/**
 * Represents a Type that you can turn into a URL.
 */
trait URLableType {
  def getURL(context:QLContext)(v:ElemValue):Option[String]
}
  
/**
 * Trait to mix into a Property that has opinions about which Links should be presented as candidates
 * in the Editor.
 */  
trait LinkCandidateProvider {
  def getLinkCandidates(state:SpaceState, currentValue:DisplayPropVal):Seq[Thing]
}

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

trait LinkUtils { self:CoreEcot =>
    
    def renderInputXmlGuts(prop:Property[_,_], state:SpaceState, currentValue:DisplayPropVal, v:ElemValue):Iterable[Elem] = {
      // Give the Property a chance to chime in on which candidates belong here:
      val candidates = prop match {
        case f:LinkCandidateProvider => f.getLinkCandidates(state, currentValue)
        case _ => state.linkCandidates(prop).toSeq.sortBy(_.displayName)
      }
      val realOptions =
        if (candidates.isEmpty) {
          Seq(<option value={UnknownOID.toString}><i>None defined</i></option>)
        } else {
          candidates map { candidate:Thing =>
            if(candidate.id == v.elem) {
              <option value={candidate.id.toString} selected="selected">{candidate.displayName}</option>        
            } else {
              <option value={candidate.id.toString}>{candidate.displayName}</option>
            }
          }
        }
      val Links = interface[querki.links.Links]
      val linkModel = prop.getPropOpt(Links.LinkModelProp)(state)
      linkModel match {
        case Some(propAndVal) => {
          val model = state.anything(propAndVal.first).get
          if (model.ifSet(Links.NoCreateThroughLinkProp)(state))
            realOptions
          else
            realOptions :+ <option class="_createNewFromModel" data-model={model.toThingId} value={UnknownOID.id.toString}>Create a New {model.displayName}</option>
        }
        case _ => realOptions
      }
    }
  
}

trait TypeCreation { self:CoreEcot with TextTypeBasis with LinkUtils =>
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
    
  /**
   * The Type for Links to other Things
   */
  class LinkType extends SystemType[OID](LinkTypeOID,
      toProps(
        setName("Link Type")
        )) with SimplePTypeBuilder[OID] with NameableType with URLableType
  {
    override def editorSpan(prop:Property[_,_]):Int = 6    
    
    def doDeserialize(v:String) = OID(v)
    def doSerialize(v:OID) = v.toString
    
    def follow(context:QLContext)(v:OID) = context.state.anything(v)
    def followLink(context:QLContext):Option[Thing] = {
      // This only works if the valType is LinkType; otherwise, it will return None
      context.value.firstAs(this).flatMap(follow(context)(_))
    }
    
    def pathAdjustments(context:QLContext):String = {
      // Find the Thing that we're actually rendering...
      val rootThingOpt = followLink(context.root)
      val adjustmentsOpt = rootThingOpt.map { rootThing =>
        val name = rootThing.toThingId.toString()
        val slashes = name.count(_ == '/')
        "../" * slashes
      }

      adjustmentsOpt.getOrElse("")
    }
    
    def makeWikiLink(context:QLContext, thing:Thing, display:Wikitext):Wikitext = {
      Wikitext("[") + display + Wikitext("](" + pathAdjustments(context) + thing.toThingId + ")")
    }

    def doWikify(context:QLContext)(v:OID, displayOpt:Option[Wikitext] = None) = {
      val target = follow(context)(v)
      val text = target match {
        case Some(t) => {
          val display = displayOpt.getOrElse(t.displayNameText.htmlWikitext)
          makeWikiLink(context, t, display)
        }
        case None => Wikitext("Bad Link: Thing " + v.toString + " not found")
      }
      text
    }
    override def doDebugRender(context:QLContext)(v:OID) = {
      val target = follow(context)(v)
      target match {
        case Some(t) => t.displayName + "(" + t.id.toThingId + ")"
        case None => "???"
      }      
    }
    
    def getNameFromId(context:QLContext)(id:OID) = {
      val tOpt = follow(context)(id)
      tOpt.map(thing => NameType.canonicalize(thing.displayName)).getOrElse(throw new Exception("Trying to get name from unknown OID " + id))      
    }
    def getName(context:QLContext)(v:ElemValue) = {
      val id = get(v)
      getNameFromId(context)(id)
    }
    
    def getURL(context:QLContext)(elem:ElemValue):Option[String] = {
      for (
        v <- elem.getOpt(this);
        thing <- follow(context)(v)
          )
        yield thing.toThingId.toString()
    }
    
    // Links are sorted by their *display names*:
    override def doComp(context:QLContext)(left:OID, right:OID):Boolean = { 
      NameType.doComp(context)(getNameFromId(context)(left), getNameFromId(context)(right))
    } 
    
    // TODO: define doFromUser()

    val doDefault = UnknownOID
    
    override def renderInputXml(prop:Property[_,_], state:SpaceState, currentValue:DisplayPropVal, v:ElemValue):Elem = {
        <select class="_linkSelect"> {
          renderInputXmlGuts(prop, state, currentValue, v)
        } </select>
    }
  }
}