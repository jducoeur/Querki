package models.system

import scala.xml._

import play.api.Logger
import play.api.templates.Html

import models._

import Thing._

import OIDs._

import ql._

import querki.ecology._
import querki.types.Types
import querki.util._
import querki.values._

trait CommonInputRenderers { self:SystemType[_] =>
  
  def renderAnyText(prop:Property[_, _], state:SpaceState, currentValue:DisplayPropVal, v:ElemValue, elemT:PType[_])(doRender: (String) => Elem):Elem = {
    val str = elemT.toUser(v)
    val xml = doRender(str)
    xml
  }
  
  def renderLargeText(prop:Property[_, _], state:SpaceState, currentValue:DisplayPropVal, v:ElemValue, elemT:PType[_]):Elem = {
    renderAnyText(prop, state, currentValue, v, elemT) { cv =>
      <textarea class="_largeTextEdit" rows="2">{cv}</textarea>
    }
  }
  
  def renderText(prop:Property[_, _], state:SpaceState, currentValue:DisplayPropVal, v:ElemValue, elemT:PType[_]):Elem = {
    renderAnyText(prop, state, currentValue, v, elemT) { cv =>
      <input type="text" value={cv}/>
    }
  }
  
  def renderBlank(prop:Property[_, _], state:SpaceState, currentValue:DisplayPropVal, elemT:PType[_]):Elem = {
    renderText(prop, state, currentValue, Core.TextType(""), Core.TextType)
  }
}

abstract class SystemType[T](tid:OID, pf:PropFetcher)(implicit e:Ecology = querki.ecology.theEcology) 
  extends PType[T](tid, systemOID, querki.core.MOIDs.RootOID, pf)(e) with CommonInputRenderers
{
  lazy val Types = interface[Types]
  
  def renderInputXml(prop:Property[_,_], state:SpaceState, currentValue:DisplayPropVal, v:ElemValue):Elem = {
    // TBD: this is smelly -- the fact that we need to know here how to render Optional is a nasty abstraction
    // break. But in general, rendering probably doesn't belong here: ultimately, rendering depends on the
    // Collection/Type matrix, and there doesn't seem to be a nice clean division of responsibilities...
    val renderedBlank = for (
      ev <- currentValue.effectiveV;
      if (displayEmptyAsBlank && ev.cType == Core.Optional && ev.isEmpty)
        )
      yield renderBlank(prop, state, currentValue, this)
      
    renderedBlank.getOrElse(renderText(prop, state, currentValue, v, this))
  }
  
  // Iff a Type wants to render QNone as blank text instead of the default value, set this to true
  val displayEmptyAsBlank:Boolean = false
}

  /**
   * The Type for integers
   */
  class IntType(tid:OID) extends SystemType[Int](tid,
      toProps(
        setName("Whole Number Type")
        )) with SimplePTypeBuilder[Int]
  {
    override val displayEmptyAsBlank:Boolean = true
    
    def doDeserialize(v:String) = try {
      java.lang.Integer.parseInt(v)
    } catch {
      case ex:java.lang.NumberFormatException => throw new PublicException("Types.Number.badFormat")
    }
    
    def doSerialize(v:Int) = v.toString
    def doWikify(context:QLContext)(v:Int, displayOpt:Option[Wikitext] = None) = Wikitext(v.toString)

    val doDefault = 0
    
    override def validate(v:String, prop:Property[_,_], state:SpaceState):Unit = {
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
  object IntType extends IntType(IntTypeOID)
  
  /**
   * The YesNo Type -- or Boolean, as us geeks think of it
   */
  class YesNoType(tid:OID) extends SystemType[Boolean](tid,
      toProps(
        setName("YesNo Type")
        )) with SimplePTypeBuilder[Boolean]
  {
    override def editorSpan(prop:Property[_,_]):Int = 1
    
    // It turns out that Java's parseBoolean is both too tolerant of nonsense, and
    // doesn't handle many common cases. So we'll do it ourselves:
    def doDeserialize(v:String) = {
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
    def doSerialize(v:Boolean) = v.toString
    def doWikify(context:QLContext)(v:Boolean, displayOpt:Option[Wikitext] = None) = Wikitext(v.toString())
    
    val doDefault = false
    
    override def renderInputXml(prop:Property[_,_], state:SpaceState, currentValue:DisplayPropVal, v:ElemValue):Elem = {
      if (get(v))
        <input type="checkbox" checked="checked" />
      else
        <input type="checkbox"/>
    }
  }
  object YesNoType extends YesNoType(YesNoTypeOID) {
    val True = YesNoType(true)
    val False = YesNoType(false)
    
    implicit def boolean2YesNo(raw:Boolean):ElemValue = {
      if (raw)
        YesNoType.True
      else
        YesNoType.False
    }
    
    // This is *begging* to crash, until YesNoType moves into Core!
    implicit def boolean2YesNoQValue(raw:Boolean):QValue = {
      Core.ExactlyOne(raw)
    }
    
    def toBoolean(typed:QValue):Boolean = {
      if (typed.pType == YesNoType)
        typed.firstAs(YesNoType).getOrElse(false)
      else
        false
    }
  }
  
  trait NameableType {
    def getName(context:QLContext)(v:ElemValue):String
  }
  
  /**
   * The Type for Display Names -- similar to Text, but not identical
   */
  abstract class NameType(tid:OID, name:String) extends SystemType[String](tid,
      toProps(
        setName(name)
        )) with SimplePTypeBuilder[String] with NameableType
  {
    def toInternal(str:String) = str.replaceAll(" ", "-")
    def toDisplay(str:String) = str.replaceAll("-", " ")
    // Note that this deliberately allows mixed-case, so that we can preserve Name case through
    // the URL for Tags. (Since the desired case *only* exists in the URL.)
    def toUrl = toInternal _
        
    def doDeserialize(v:String) = toDisplay(v)
    def doSerialize(v:String) = toInternal(v)
    
    override def doToUser(v:String):String = toDisplay(v)
    override protected def doFromUser(v:String):String = {
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
    
    def equalNames(str1:String, str2:String):Boolean = {
      canonicalize(str1).contentEquals(canonicalize(str2))
    }
    
    def canonicalize(str:String):String = toInternal(str).toLowerCase
    
    def makeLegal(str:String):String = str.filter(c => c.isLetterOrDigit || c == ' ' || c == '-')
    
    def getName(context:QLContext)(v:ElemValue) = canonicalize(get(v))
    
    def nameToLink(context:QLContext)(v:String, displayOpt:Option[Wikitext] = None) = {
      val display = displayOpt.getOrElse(Wikitext(v))
      Wikitext("[") + display + Wikitext("](" + toUrl(v) + ")")
    }
    
    override def doComp(context:QLContext)(left:String, right:String):Boolean = { left < right } 

    val doDefault = ""
      
    override def doMatches(left:String, right:String):Boolean = equalNames(left, right)
  }
  object NameType extends NameType(NameTypeOID, "Name Type") {
    override def editorSpan(prop:Property[_,_]):Int = 3
    
    def doWikify(context:QLContext)(v:String, displayOpt:Option[Wikitext] = None) = Wikitext(toDisplay(v))    
  }
  object TagSetType extends NameType(TagSetOID, "Tag Set Type") {
    override def editorSpan(prop:Property[_,_]):Int = 12
    
    override def requiredColl:Option[Collection] = Some(Core.QSet)
    
    // TODO: this should probably get refactored with LinkType? They're different ways of
    // expressing the same concepts; it's just that Links are OID-based, whereas Names/Tags are
    // name-based.
    def doWikify(context:QLContext)(v:String, displayOpt:Option[Wikitext] = None) = nameToLink(context)(v)
    
    override def renderProperty(prop:Property[_,_])(implicit request:RequestContext):Option[Wikitext] = {
      val parser = new ql.QLParser(querki.core.QLText("""These tags are currently being used:
[[_tagsForProperty -> _sort -> _bulleted]]"""), prop.thisAsContext)
      Some(parser.process)
    }
  }

object SystemTypes {
  def all = OIDMap[PType[_]](
      IntType, YesNoType, NameType, TagSetType)  
}