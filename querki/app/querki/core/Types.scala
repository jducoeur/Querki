package querki.core

import scala.xml.NodeSeq

import models.{DisplayPropVal, OID, Property, PType, PTypeBuilder, PTypeBuilderBase, SimplePTypeBuilder, Thing, UnknownOID, Wikitext}
import models.Thing.PropFetcher

import querki.ecology._

import querki.ql.QLPhrase
import querki.util.PublicException
import querki.values.{ElemValue, QLContext, QValue, RequestContext, SpaceState}

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
  
  lazy val QL = interface[querki.ql.QL]
  
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
    def doDeserialize(v:String)(implicit state:SpaceState) = QLText(v)
    def doSerialize(v:QLText)(implicit state:SpaceState) = v.text
    def doWikify(context:QLContext)(v:QLText, displayOpt:Option[Wikitext] = None) = {
      QL.process(v, context)
    }
    def doDefault(implicit state:SpaceState) = QLText("")
    def wrap(raw:String):valType = QLText(raw)
    
    override def validate(v:String, prop:Property[_,_], state:SpaceState):Unit = validateText(v, prop, state)

    // TBD: in principle, we really want this to return a *context*, not a *value*. This is a special
    // case of a growing concern: that we could be losing information by returning QValue from
    // qlApply, and should actually be returning a full successor Context.
    // TODO: merge this with the fairly-similar code in QLType
    override def qlApplyFromProp(inv:Invocation, prop:Property[QLText,_]):Option[QValue] = {
      val definingContext = inv.definingContext.getOrElse(inv.context)
      // TBD: in a perfect world, this would be a true Warning: legal, but with a warning on the side. Unfortunately, doing it
      // like this gets in the way of perfectly legit ordinary situations, and violates the general Querki data model, that
      // passing None to a Stage usually results in None. (That is, it violates ordinary map semantics.)
//      if (definingContext.isEmpty) {
//        Some(WarningValue("""Trying to use Text Property """" + prop.displayName + """" in an empty context.
//This often means that you've invoked it recursively without saying which Thing it is defined in."""))
//      } else {
        val ParsedTextType = interface[querki.ql.QL].ParsedTextType
        // TODO: this is a good candidate for optimization. If we turned this inside-out, we could fetch the QLText
        // from the definingContext, parse that *once*, and process that AST over each element of the incomingContext.
        // That would potentially save us tons and tons of redundant parsing. But it requires reworking the QLParser
        // interface, to split parsing from processing.
        Some(inv.context.collect(ParsedTextType) { elemContext:QLContext =>
          prop.applyToIncomingProps(definingContext) { (thing, context) =>
            implicit val s = definingContext.state
            // In other words, map over all the text values in this property, parsing all of them
            // and passing the resulting collection along the pipeline:
            thing.map(prop, ParsedTextType) { qlText =>
              QL.process(qlText, elemContext, Some(inv))
            }
          }
        })
//      }
    }
      
    def code(elem:ElemValue):String = get(elem).text
  }
}
  
trait NameableType {
  def getName(context:QLContext)(v:ElemValue):String
}

trait NameUtils {
  def toInternal(str:String) = str.replaceAll(" ", "-")
  def toDisplay(str:String) = str.replaceAll("-", " ")
  // Note that this deliberately allows mixed-case, so that we can preserve Name case through
  // the URL for Tags. (Since the desired case *only* exists in the URL.)
  def toUrl = toInternal _
  
  def canonicalize(str:String):String = toInternal(str).toLowerCase  
  
  def compareNames(context:QLContext)(left:String, right:String):Boolean = { left < right } 
      
  def makeLegal(str:String):String = str.filter(c => c.isLetterOrDigit || c == ' ' || c == '-')
    
  def equalNames(str1:String, str2:String):Boolean = {
    canonicalize(str1).contentEquals(canonicalize(str2))
  }
}
// So that these functions can be used without mixing in NameUtils:
object NameUtils extends NameUtils

// Marker trait for NameTypeBase and everything that descends from it:
trait IsNameType extends PType[String] 

trait NameTypeBasis { self:CoreEcot with NameUtils =>  
  /**
   * The Type for Display Names -- similar to Text, but not identical
   */
  abstract class NameTypeBase(tid:OID, pf:PropFetcher) extends SystemType[String](tid, pf) 
    with SimplePTypeBuilder[String] with NameableType with IsNameType
  {
        
    def doDeserialize(v:String)(implicit state:SpaceState) = toDisplay(v)
    def doSerialize(v:String)(implicit state:SpaceState) = toInternal(v)
    
    override def doToUser(v:String)(implicit state:SpaceState):String = toDisplay(v)
    override protected def doFromUser(v:String)(implicit state:SpaceState):String = {
      if (v.length() == 0)
        throw new PublicException("Types.Name.empty")
      else if (v.length() > 254)
        throw new PublicException("Types.Name.tooLong")
      // TODO: this should forbid double-slashes, since "//" is a comment in QL. Possibly
      // we should unify this with the QLParser.name regex?
      else if (v.exists(c => !c.isLetterOrDigit && c != '-' && c != ' ' && c != '/'))
        throw new PublicException("Types.Name.badChars")
      else
        toDisplay(v)
    }
    
    def getName(context:QLContext)(v:ElemValue) = canonicalize(get(v))
    
    def nameToLink(context:QLContext)(v:String, displayOpt:Option[Wikitext] = None) = {
      val display = displayOpt.getOrElse(Wikitext(v))
      Wikitext("[") + display + Wikitext("](" + toUrl(v) + ")")
    }
    
    override def doComp(context:QLContext)(left:String, right:String):Boolean = compareNames(context)(left,right)

    def doDefault(implicit state:SpaceState) = ""
      
    override def doMatches(left:String, right:String):Boolean = equalNames(left, right)
  }  
}

trait LinkUtils { self:CoreEcot =>
    
    def renderInputXmlGuts(prop:Property[_,_], rc:RequestContext, currentValue:DisplayPropVal, v:ElemValue):NodeSeq = {
      val state = rc.state.get
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
        case Some(propAndVal) if (!propAndVal.isEmpty) => {
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

trait TypeCreation { self:CoreEcot with BootUtils with TextTypeBasis with NameTypeBasis with LinkUtils with NameUtils =>

  /**
   * Marker type, used to signify "no real type" in empty collections.
   */
  class UnknownType extends PType[Unit](UnknownOID, UnknownOID, UnknownOID, () => models.Thing.emptyProps) {
    def doDeserialize(v:String)(implicit state:SpaceState) = throw new Exception("Trying to use UnknownType!")
    def doSerialize(v:Unit)(implicit state:SpaceState) = throw new Exception("Trying to use UnknownType!")
    def doWikify(context:QLContext)(v:Unit, displayOpt:Option[Wikitext] = None) = throw new Exception("Trying to use UnknownType!")
  
    def renderInputXml(prop:Property[_,_], rc:RequestContext, currentValue:DisplayPropVal, v:ElemValue):NodeSeq = 
      throw new Exception("Trying to use UnknownType!")

    def doDefault(implicit state:SpaceState) = throw new Exception("Trying to use UnknownType!")
  }
  
  /**
   * The Root Type, that all others are based on.
   * 
   * Note that this was a late addition to the system, and is necessary mostly for Model Types to hang off of.
   */
  class UrType extends PType[Unit](UrTypeOID, SystemIds.systemOID, querki.core.MOIDs.RootOID, 
      toProps(
        setName("Root Type"),
        setInternal))
  {
    def doDeserialize(v:String)(implicit state:SpaceState) = throw new Exception("Trying to use UrType!")
    def doSerialize(v:Unit)(implicit state:SpaceState) = throw new Exception("Trying to use UrType!")
    def doWikify(context:QLContext)(v:Unit, displayOpt:Option[Wikitext] = None) = throw new Exception("Trying to use UrType!")
  
    def renderInputXml(prop:Property[_,_], rc:RequestContext, currentValue:DisplayPropVal, v:ElemValue):NodeSeq = 
      throw new Exception("Trying to use UrType!")

    def doDefault(implicit state:SpaceState) = throw new Exception("Trying to use UrType!")    
  }
  
  class InternalMethodType extends SystemType[String](InternalMethodOID,
    toProps(
      setName("Internal Method Type"),
      setInternal
    )) with SimplePTypeBuilder[String]
  {
    def boom = throw new Exception("InternalMethodType cannot be used conventionally. It simply wraps code.")
    def doDeserialize(v:String)(implicit state:SpaceState) = boom
    def doSerialize(v:String)(implicit state:SpaceState) = boom

    def doWikify(context:QLContext)(v:String, displayOpt:Option[Wikitext] = None) = Wikitext("Internal Method")
    
    def doDefault(implicit state:SpaceState) = ""
    override def wrap(raw:String):valType = boom 
  }
  
  class NameType extends NameTypeBase(NameTypeOID, 
      toProps(setName("Name Type"))) {
    override def editorSpan(prop:Property[_,_]):Int = 3
    
    def doWikify(context:QLContext)(v:String, displayOpt:Option[Wikitext] = None) = Wikitext(toDisplay(v))    
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
    
    override def renderInputXml(prop:Property[_,_], rc:RequestContext, currentValue:DisplayPropVal, v:ElemValue):NodeSeq =
      renderLargeText(prop, rc, currentValue, v, this)
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
    
    def doDeserialize(v:String)(implicit state:SpaceState) = OID(v)
    def doSerialize(v:OID)(implicit state:SpaceState) = v.toString
    
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
      tOpt.map(thing => canonicalize(thing.displayName)).getOrElse(throw new Exception("Trying to get name from unknown OID " + id))      
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
      compareNames(context)(getNameFromId(context)(left), getNameFromId(context)(right))
    } 
    
    // TODO: define doFromUser()

    def doDefault(implicit state:SpaceState) = UnknownOID
    
    override def renderInputXml(prop:Property[_,_], rc:RequestContext, currentValue:DisplayPropVal, v:ElemValue):NodeSeq = {
        <select class="_linkSelect"> {
          renderInputXmlGuts(prop, rc, currentValue, v)
        } </select>
    }
    
    override def doToUser(v:OID)(implicit state:SpaceState):String = {
      state.anything(v) match {
        case Some(thing) => thing.displayName
        case None => v.toString
      }
    }
  }
  
  /**
   * The Type for integers
   */
  class IntType extends SystemType[Int](IntTypeOID,
      toProps(
        setName("Whole Number Type")
        )) with SimplePTypeBuilder[Int]
  {
    override val displayEmptyAsBlank:Boolean = true
    
    def doDeserialize(v:String)(implicit state:SpaceState) = try {
      java.lang.Integer.parseInt(v)
    } catch {
      case ex:java.lang.NumberFormatException => throw new PublicException("Types.Number.badFormat")
    }
    
    def doSerialize(v:Int)(implicit state:SpaceState) = v.toString
    def doWikify(context:QLContext)(v:Int, displayOpt:Option[Wikitext] = None) = Wikitext(v.toString)

    def doDefault(implicit state:SpaceState) = 0
    
    override def validate(v:String, prop:Property[_,_], state:SpaceState):Unit = {
      implicit val s = state
      for (
        minValPO <- prop.getPropOpt(Types.MinIntValueProp)(state);
        minVal <- minValPO.firstOpt;
        if (doDeserialize(v) < minVal)
          )
        throw new PublicException("Types.Int.tooLow", prop.displayName, minVal)
      
      for (
        maxValPO <- prop.getPropOpt(Types.MaxIntValueProp)(state);
        maxVal <- maxValPO.firstOpt;
        if (doDeserialize(v) > maxVal)
          )
        throw new PublicException("Types.Int.tooHigh", prop.displayName, maxVal)
    }  
    
   override def editorSpan(prop:Property[_,_]):Int = 1    
    /**
     * TODO: eventually, we may want a more nuanced Int inputter. But this will do to start.
     */
  }
  
  /**
   * The YesNo Type -- or Boolean, as us geeks think of it
   */
  class YesNoType extends SystemType[Boolean](YesNoTypeOID,
      toProps(
        setName("YesNo Type")
        )) with SimplePTypeBuilder[Boolean]
  {
    override def editorSpan(prop:Property[_,_]):Int = 1
    
    // It turns out that Java's parseBoolean is both too tolerant of nonsense, and
    // doesn't handle many common cases. So we'll do it ourselves:
    def doDeserialize(v:String)(implicit state:SpaceState) = {
      v.toLowerCase() match {
        case "true" => true
        case "false" => false
        
        case "1" => true
        case "0" => false
        
        case "yes" => true
        case "no" => false
        
        case "on" => true
        case "off" => false
        
        case "t" => true
        case "f" => false
        
        case "y" => true
        case "n" => false
        
        // Okay, this one looks odd, but it relates to the way checkboxes get handled in HTTP. If the checkbox
        // is empty (false), then it *is not transmitted*! If it is checked (true), then it gets transmitted,
        // but its value is sometimes left empty; the fact that it is sent at all means that it is true.
        // TBD: this is kind of idiotic. Why is it inconsistently happening?
        case "" => true
        
        case _ => throw new Exception("I can't interpret " + v + " as a YesNo value")
      }
    }
    def doSerialize(v:Boolean)(implicit state:SpaceState) = v.toString
    def doWikify(context:QLContext)(v:Boolean, displayOpt:Option[Wikitext] = None) = Wikitext(v.toString())
    
    def doDefault(implicit state:SpaceState) = false
    
    override def renderInputXml(prop:Property[_,_], rc:RequestContext, currentValue:DisplayPropVal, v:ElemValue):NodeSeq = {
      if (get(v))
        <input type="checkbox" checked="checked" />
      else
        <input type="checkbox"/>
    }
  }
}
