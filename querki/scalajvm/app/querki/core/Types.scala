package querki.core

import scala.xml.NodeSeq

import models.{DelegatingType, DisplayPropVal, Kind, OID, Property, PropertyBundle, PType, PTypeBuilder, PTypeBuilderBase, SimplePTypeBuilder, Thing, UnknownOID, Wikitext}
import models.Thing.PropFetcher

import querki.ecology._

import querki.ql.QLPhrase
import querki.util.{PublicException, QLog}
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
  def getDisplay(context:QLContext)(v:ElemValue):Option[String]
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
    private lazy val Core = interface[querki.core.Core]
    
    def doDeserialize(v:String)(implicit state:SpaceState) = QLText(v)
    def doSerialize(v:QLText)(implicit state:SpaceState) = v.text
    def doWikify(context:QLContext)(v:QLText, displayOpt:Option[Wikitext] = None, lexicalThing:Option[PropertyBundle] = None) = {
      QL.process(v, context, lexicalThing = lexicalThing)
    }
    def doDefault(implicit state:SpaceState) = QLText("")
    def wrap(raw:String):valType = QLText(raw)
    
    override def validate(v:String, prop:Property[_,_], state:SpaceState):Unit = validateText(v, prop, state)

    override def qlApplyFromProp(inv:Invocation, prop:Property[QLText,_]):Option[QValue] = {
      implicit val s = inv.state
      val result:QValue = for {
        // Declare the expected return type, which can be important if the results are empty and get
        // fed into, eg, _sort.
        // TODO: this should become a more general concept! We should be declaring this transform at
        // the Type level, I think.
        dummy <- inv.returnsType(QL.ParsedTextType)
        // For each received Thing (note that the bundle is where the prop is defined on, which may be
        // different from the elemContext!)...
        (bundle, elemContext) <- inv.bundlesAndContextsForProp(prop)
        // ... get this Property's value on the Thing...
        pv <- inv.opt(bundle.getPropOpt(prop))
        // ... tell the system which Collection we expect to produce...
        dummy2 <- inv.preferCollection(pv.v.cType)
        // ... get each element in the Property (which can be multi-valued -- for example, a List of Text)...
        qlText <- inv.iter(pv.v.rawList(this))
      }
        // ... and process that element through QL.
        yield Core.ExactlyOne(QL.ParsedTextType(QL.process(qlText, elemContext, Some(inv), Some(bundle), Some(prop))))
        
      Some(result)
    }
      
    def code(elem:ElemValue):String = get(elem).text
  }
}
  
trait NameableType {
  def getName(context:QLContext)(v:ElemValue):String
}

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
    
    override def doComp(context:QLContext)(left:String, right:String):Boolean = compareNames(left,right)

    def doDefault(implicit state:SpaceState) = ""
      
    override def doMatches(left:String, right:String):Boolean = equalNames(left, right)
    
    override def canCoerceTo(other:PType[_]):Boolean = {
      other.isInstanceOf[IsTextType] || other == QL.ParsedTextType
    }
    override def coerceTo(other:PType[_], elem:ElemValue):ElemValue = {
      if (other == QL.ParsedTextType)
        QL.ParsedTextType(Wikitext(get(elem)))
      else other match {
        case tt:IsTextType => new ElemValue(QLText(get(elem)), other)
        case _ => throw new Exception(s"Can not coerce NameTypeBase to ${other.displayName}")
      }
    }
  }  
}

trait IntTypeBasis { self:CoreEcot =>
  /**
   * The Type for integers
   */
  class IntTypeBase(tid:OID, pf:PropFetcher) extends SystemType[Int](tid, pf) with SimplePTypeBuilder[Int]
  {
    override val displayEmptyAsBlank:Boolean = true
    
    def doDeserialize(v:String)(implicit state:SpaceState) = try {
      java.lang.Integer.parseInt(v)
    } catch {
      case ex:java.lang.NumberFormatException => throw new PublicException("Types.Int.badFormat")
    }
    
    def doSerialize(v:Int)(implicit state:SpaceState) = v.toString
    def doWikify(context:QLContext)(v:Int, displayOpt:Option[Wikitext] = None, lexicalThing:Option[PropertyBundle] = None) = Wikitext(v.toString)

    def doDefault(implicit state:SpaceState) = 0
    
    override def validate(v:String, prop:Property[_,_], state:SpaceState):Unit = {
      implicit val s = state
      if (v.length == 0)
        throw new PublicException("Types.Int.empty")
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
    
   override def doComp(context:QLContext)(left:Int, right:Int):Boolean = { left < right } 
    
   override def editorSpan(prop:Property[_,_]):Int = 1    
    /**
     * TODO: eventually, we may want a more nuanced Int inputter. But this will do to start.
     */
  }
}

trait LinkUtils { self:CoreEcot =>
  
  def InternalProp:Property[Boolean,Boolean]
  def Links:querki.links.Links
  
  /**
   * Given a Link Property, return all of the appropriate candidates for that property to point to.
   * 
   * The Property passed into here should usually be of LinkType -- while in theory that's not required,
   * it would be surprising for it to be used otherwise.
   */
  def linkCandidates(state:SpaceState, Links:querki.links.Links, prop:Property[_,_]):Seq[Thing] = {
    implicit val s = state
    
    val locals = linkCandidatesLocal(state, Links, prop)
    if (state.app.isDefined && prop.hasProp(Links.LinkAllowAppsProp) && prop.first(Links.LinkAllowAppsProp))
      locals ++: linkCandidates(state.app.get, Links, prop)
    else
      locals
  }

  /**
   * This enumerates all of the plausible candidates for the given property within this Space.
   */
  def linkCandidatesLocal(state:SpaceState, Links:querki.links.Links, prop:Property[_,_]):Seq[Thing] = {
    implicit val s = state
    
    // First, filter the candidates based on LinkKind:
    val allCandidatesIt = if (prop.hasProp(Links.LinkKindProp)) {
      val allowedKinds = prop.getPropVal(Links.LinkKindProp).cv
      def fetchKind(wrappedVal:ElemValue):Iterable[Thing] = {
        val kind = Links.LinkKindProp.pType.get(wrappedVal)
        kind match {
          case Kind.Thing => state.things.values
          case Kind.Property => state.spaceProps.values
          case Kind.Type => state.types.values
          case Kind.Collection => state.colls.values
          case _ => Iterable.empty[Thing]
        }
      }
      (Iterable.empty[Thing] /: allowedKinds)((it, kind) => it ++: fetchKind(kind))
    } else {
      // No LinkKind specified, so figure that they only want Things:
      state.things.values
    }
    val allCandidates = allCandidatesIt.toSeq
    
    // Now, if they've specified a particular Model to be the limit of the candidate
    // tree -- essentially, they've specified what type you can link to -- filter for
    // that:
    // TODO: we really should be building and caching the entire hierarchy, and if there
    // is a LinkModelProp just use the children of that. As it stands, that isAncestor() call
    // is n**2 (maybe worse) in context.
    val filteredByModel = if (prop.hasProp(Links.LinkModelProp)) {
      prop.firstOpt(Links.LinkModelProp) match {
        case Some(modelId) => {
          val explicitChoices = for {
            model <- state.anything(modelId)
            pv <- model.getPropOpt(Links.ChoiceOrderProp)
            instanceIds = pv.rawList
          }
            yield instanceIds.map(state.anything(_)).flatten
          
          explicitChoices.getOrElse(allCandidates filter (_.isAncestor(modelId)) sortBy (_.displayName))
        }
        case None => allCandidates sortBy (_.displayName)
      }
    } else {
      allCandidates sortBy (_.displayName)
    }
    
    val filteredAsModel = if (prop.ifSet(Links.LinkToModelsOnlyProp)) {
      filteredByModel filter (_.isModel)
    } else {
      filteredByModel
    }
    
    filteredAsModel.filterNot(_.ifSet(InternalProp))
  }    
  
    def renderInputXmlGuts(prop:Property[_,_], context:QLContext, currentValue:DisplayPropVal, v:ElemValue, allowEmpty:Boolean):NodeSeq = {
      <select class="_linkSelect"> 
      {
      val state = context.state
      val Links = interface[querki.links.Links]
      // Give the Property a chance to chime in on which candidates belong here:
      val candidates = prop match {
        case f:LinkCandidateProvider => f.getLinkCandidates(state, currentValue)
        case _ => linkCandidates(state, Links, prop)
      }
      val realOptions =
        if (candidates.isEmpty) {
          Seq(<option value={UnknownOID.toString}><i>None defined</i></option>)
        } else {
          // Note: the unsafeDisplayNames below are because Scala's XML interpolator appears to be doing the
          // name sanitizing for us:
          candidates map { candidate:Thing =>
            val name = context.requestOpt.map(candidate.unsafeNameOrComputed(_, state)).getOrElse(candidate.unsafeDisplayName)
            if(candidate.id == v.elem) {
              <option value={candidate.id.toString} selected="selected">{name}</option>        
            } else {
              <option value={candidate.id.toString}>{name}</option>
            }
          }
        }
      val withOpt =
        if (allowEmpty)
          <option value={UnknownOID.id.toString}>Nothing selected</option> +: realOptions
        else
          realOptions
      val linkModel = prop.getPropOpt(Links.LinkModelProp)(state)
      linkModel match {
        case Some(propAndVal) if (!propAndVal.isEmpty) => {
          val model = state.anything(propAndVal.first).get
          if (model.ifSet(Links.NoCreateThroughLinkProp)(state))
            withOpt
          else
            withOpt :+ <option class="_createNewFromModel" data-model={model.toThingId} value={UnknownOID.id.toString}>Create a New {model.displayName}</option>
        }
        case _ => withOpt
      }
      } </select>
    }
  
}

trait CoreExtra {
  def Summary(text:String):(OID,QValue)
  def Details(text:String):(OID,QValue)
  def setInternal:(OID, QValue)
}

trait TypeCreation { self:CoreEcot with TextTypeBasis with NameTypeBasis with IntTypeBasis with LinkUtils with NameUtils with Core with CoreExtra =>

  /**
   * Marker type, used to signify "no real type" in empty collections.
   */
  class UnknownType extends PType[Unit](UnknownOID, UnknownOID, UnknownOID, toProps(setName("Unknown Type"))) {
    def doDeserialize(v:String)(implicit state:SpaceState) = throw new Exception("Trying to use UnknownType!")
    def doSerialize(v:Unit)(implicit state:SpaceState) = throw new Exception("Trying to use UnknownType!")
    def doWikify(context:QLContext)(v:Unit, displayOpt:Option[Wikitext] = None, lexicalThing:Option[PropertyBundle] = None) = throw new Exception("Trying to use UnknownType!")
  
    def renderInputXml(prop:Property[_,_], context:QLContext, currentValue:DisplayPropVal, v:ElemValue):NodeSeq = 
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
        (querki.conventions.MOIDs.PropSummaryOID -> 
            ExactlyOne(ElemValue(
                QLText("The Ur-Type, from which all others descend"), 
                new DelegatingType(TextType)))),
        (querki.conventions.MOIDs.PropDetailsOID -> 
        	ExactlyOne(ElemValue(QLText("""Querki Types are just like any other Things -- they have Models and Instances.
        	    |The Root Type is essentially the Model for all the other Types. It must never be used directly.""".stripMargin),
            new DelegatingType(LargeTextType)))),
        setInternal))
  {
    def doDeserialize(v:String)(implicit state:SpaceState) = throw new Exception("Trying to use UrType!")
    def doSerialize(v:Unit)(implicit state:SpaceState) = throw new Exception("Trying to use UrType!")
    def doWikify(context:QLContext)(v:Unit, displayOpt:Option[Wikitext] = None, lexicalThing:Option[PropertyBundle] = None) = throw new Exception("Trying to use UrType!")
  
    def renderInputXml(prop:Property[_,_], context:QLContext, currentValue:DisplayPropVal, v:ElemValue):NodeSeq = 
      throw new Exception("Trying to use UrType!")

    def doDefault(implicit state:SpaceState) = throw new Exception("Trying to use UrType!")    
  }
  
  class InternalMethodType extends SystemType[String](InternalMethodOID,
    toProps(
      setName("Internal Method Type"),
      setInternal,
      Summary("A system-created Function"),
      Details("""A system-created Function. You can not create these, and generally shouldn't worry about this Type.""".stripMargin)
    )) with SimplePTypeBuilder[String]
  {
    def boom = throw new Exception("InternalMethodType cannot be used conventionally. It simply wraps code.")
    def doDeserialize(v:String)(implicit state:SpaceState) = boom
    def doSerialize(v:String)(implicit state:SpaceState) = boom

    def doWikify(context:QLContext)(v:String, displayOpt:Option[Wikitext] = None, lexicalThing:Option[PropertyBundle] = None) = Wikitext("Internal Method")
    
    def doDefault(implicit state:SpaceState) = ""
    override def wrap(raw:String):valType = boom 
  }
  
  class NameType extends NameTypeBase(NameTypeOID, 
      toProps(
        setName("Name Type"),
        Summary("A Name that may or may not be on a Thing"),
        Details("""A "Name" is exactly that -- a name that can be applied to a Thing. It does not
            |necessarily mean a name that is currently in use: the Name can be for a Thing that already
            |exists, or it can simply be a Name with no actual Thing named by it yet.
            |
            |Names are very restricted: they can contain only letter, numbers, spaces, dashes and underscores.
            |User-defined names may not start with underscore. (System-defined names often do.)
            |
            |Any given Space may contain at most one Thing with any given Name -- you can't duplicate Names.
            |
            |When you choose "Name Type", the system will say "Link to which Model?". This is optional, but sometimes
            |helpful. When you are editing a Name Property, the system will prompt you with existing Names. If you
            |choose a Model at creation time, it will only prompt you with Names of Instances of that Model. So if
            |you know what sorts of Things you will be naming in this Property, it is worth specifying that Model here.
            |
            |Note that Names are different from the more-common Display Names, which you will usually use. These
            |have far fewer restrictions, and are usually what you will see in practice. Most of the time, a Thing's
            |Name is derived automatically from its Display Name when the Thing is first created, by stripping
            |out the illegal characters.
            |
            |Names are used mainly to generate the URLs for each Thing, and are therefore sometimes called Link Names.
            |
            |Names are fairly advanced -- most users usually won't want to create a Name Property.""".stripMargin))) 
  {
    override def editorSpan(prop:Property[_,_]):Int = 3
    
    def doWikify(context:QLContext)(v:String, displayOpt:Option[Wikitext] = None, lexicalThing:Option[PropertyBundle] = None) = Wikitext(toDisplay(v))    
  }

  /**
   * The Type for Text -- probably the most common type in Querki
   */
  class TextType extends TextTypeBase(TextTypeOID,
      toProps(
        setName("Text Type"),
        (querki.conventions.MOIDs.PropSummaryOID -> 
            ExactlyOne(ElemValue(
                QLText("A single line of text, which may contain QL expressions"), 
                new DelegatingType(TextType)))),
        (querki.conventions.MOIDs.PropDetailsOID -> 
        	ExactlyOne(ElemValue(QLText("""Text Type is almost the same as Large Text Type -- see Large Text Type for most of the details.
            |
            |The only real difference is that the input field for a Text Type Property is only a single line, and
            |Text Properties are usually intended to be relatively short. As a rule of thumb, if you can imagine
            |this Property ever holding more than a paragraph, use Large Text instead.""".stripMargin),
            new DelegatingType(LargeTextType))))
        )) with PTypeBuilder[QLText,String] 
  {
    override def editorSpan(prop:Property[_,_]):Int = 12    
  }

  /**
   * The Type for Large Text -- stuff that we expect to take up more space on-screen
   */
  class LargeTextType extends TextTypeBase(LargeTextTypeOID,
      toProps(
        setName("Large Text Type"),
        (querki.conventions.MOIDs.PropSummaryOID -> 
            ExactlyOne(ElemValue(
                QLText("A block of text, which may contain QL expressions"),
                new DelegatingType(TextType)))),
        (querki.conventions.MOIDs.PropDetailsOID -> 
        	ExactlyOne(ElemValue(QLText("""Large Text is one of the central Types in Querki: it is an arbitrarily long block of text,
            |which may contain QL expressions in it. Most Querki Spaces use Large Text Properties frequently.
            |
            |When you edit a Large Text, you will see a multi-line input box. This will grow automatically as
            |you enter more text, and can get pretty much as large as you like. (There are limits, but they
            |are pretty substantial -- multi-page Large Texts are not unusual.)
            |
            |Every Thing automatically has one Large Text Property available, named Default View. This is what
            |gets shown when you simply look at this Thing in the browser. Most of the time, you want to fill in
            |the Default View on your Model, with a page that includes all the interesting Properties on the Model.
            |Then, all of the Instances of that Model will automatically pick up that Default View, and look right.
            |
            |Large Text is the most-commonly used text type in Querki, but there are a couple of others. Text Type
            |is identical to Large Text, just smaller -- it only has a one-line input box, and is intended for short
            |texts of a paragraph or less. Plain Text can not contain QL expressions, so it is only for simple, literal
            |blocks of text.
            |
            |#### QText
            |
            |Large Text Properties are mainly composed of QText -- an easy-to-use "markup" format, that lets you
            |describe concepts like boldface, paragraphs, links, bullet lists and so on in fairly intuitive ways, without
            |needing to use HTML. For details on all the different things you can do with QText, see
            |the [QText Reference](http://www.querki.net/u/systemUser/documentation/QText-Reference).
            |
            |#### QL
            |
            |A Large Text Property may also contain QL expressions. These are simple expressions contained in
            |double-square-brackets. The tutorial for QL is still being written, but for new users, you really only
            |need two kinds of QL expressions to get going.
            |
            |First, say that your Space has a page named Instructions, and you want another page to link to that.
            |You show that link by simply saying:
            |```
            |\[[Instructions\]]
            |```
            |It's as easy as that, but keep in mind that you need to use the other page's Link Name, which may be
            |slightly different from its Display Name. If you're not sure, look in the small subtitle line on
            |that page -- it should give the "Link Name", which is what you should use.
            |
            |Second -- Querki is all about Things with Properties. Say that you want to show the value of the Property
            |named "Details" on your Thing. You would just say:
            |```
            |\[[Details\]]
            |```
            |That's it -- Querki is smart enough to know that, since you're naming a Property, it should just insert the
        	|value of that Property here.""".stripMargin),
            new DelegatingType(LargeTextType))))
        )) with PTypeBuilder[QLText,String] 
  {
    override def editorSpan(prop:Property[_,_]):Int = 12
    
    override def renderInputXml(prop:Property[_,_], context:QLContext, currentValue:DisplayPropVal, v:ElemValue):NodeSeq =
      renderLargeText(prop, context, currentValue, v, this)
  }
    
  /**
   * The Type for Links to other Things
   */
  class LinkType extends SystemType[OID](LinkTypeOID,
      toProps(
        setName("Link Type"),
        Summary("A pointer to a specific Thing"),
        Details("""A Link points to a single Thing in this Space. It can point to any Thing: an Instance, a Model,
            |a Property -- even a Type or the Space itself.
            |
            |The editing interface for Links is currently very different for a Set of Links (which is almost identical
            |to that for a Tag Set), as opposed to a single Link. But under the hood, they're both letting you do the
            |same thing: choose one or more Things.
            |
            |When you choose "Link Type", the system will say "Link to which Model?". This is optional, but usually
            |helpful. When you are editing a Link Property, the system will prompt you with existing Things. If you
            |choose a Model at creation time, it will only prompt you with existing Instances of that Model. So if
            |you know what sorts of Things you will be using in this Property, it is worth specifying that Model here.            
            |""".stripMargin)
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
    
    def makeWikiLink(context:QLContext, thing:Thing, display:Wikitext):Wikitext = {
      Wikitext("[") + display + Wikitext("](" + thing.toThingId + ")")
    }

    def doWikify(context:QLContext)(v:OID, displayOpt:Option[Wikitext] = None, lexicalThing:Option[PropertyBundle] = None) = {
      val target = follow(context)(v)
      val text = target match {
        case Some(t) => {
          val display = displayOpt.getOrElse(context.requestOpt.map(t.nameOrComputed(_, context.state)).getOrElse(t.displayNameText).htmlWikitext)
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
    
    def getDisplay(context:QLContext)(elem:ElemValue):Option[String] = {
      for (
        v <- elem.getOpt(this);
        thing <- follow(context)(v)
          )
        yield thing.displayName
    }
    
    // Links are sorted by their *display names*:
    override def doComp(context:QLContext)(left:OID, right:OID):Boolean = { 
      compareNames(getNameFromId(context)(left), getNameFromId(context)(right))
    } 
    
    // TODO: define doFromUser()

    def doDefault(implicit state:SpaceState) = UnknownOID
    
    override def renderInputXml(prop:Property[_,_], context:QLContext, currentValue:DisplayPropVal, v:ElemValue):NodeSeq = {
      renderInputXmlGuts(prop, context, currentValue, v, false)
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
  class IntType extends IntTypeBase(IntTypeOID,
      toProps(
        setName("Whole Number Type"),
        Summary("A number"),
        Details("""A Whole Number Property contains a number with no fractional part. That is, you can
            |give 0, 12, or 309298474, but you can't say 14.98 or 9 3/4. (Floating point and fractional
            |numbers will come eventually -- if you have a serious need for them, please raise it as an issue.)
            |
            |These sorts of numbers are called "Integers" in most programming languages.
            |
            |If your Property is Optional Whole Number, you may leave the input empty (simply backspace out
            |to clear the number) to indicate that you don't have any number to put here.
            |
            |At the moment, Querki only accepts positive numbers, not negative ones. If you need to use
            |negative numbers, please raise it as an issue. (It isn't hard to add, but hasn't come up as
            |a high priority yet.)""".stripMargin)
        ))
  
  /**
   * The YesNo Type -- or Boolean, as us geeks think of it
   */
  class YesNoType extends SystemType[Boolean](YesNoTypeOID,
      toProps(
        setName("YesNo Type"),
        Summary("A yes/no, true/false value"),
        Details("""A YesNo Property allows you to say whether something is true or false.
            |
            |If you say that your Property is Optional YesNo, that essentially introduces the
            |concept of "Maybe" -- a value that is neither True nor False.
            |
            |YesNo values are called "Boolean" in most programming languages; we are deliberately
            |avoiding that particular bit of computer-science jargon.""".stripMargin)
        )) with SimplePTypeBuilder[Boolean]
  {
    // This is horribly hackish, and illustrates that editorSpan is more than just a function of
    // the PType:
    override def editorSpan(prop:Property[_,_]):Int = {
      if (prop.cType == Optional)
        2
      else
        1
    }
    
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
    def doWikify(context:QLContext)(v:Boolean, displayOpt:Option[Wikitext] = None, lexicalThing:Option[PropertyBundle] = None) = Wikitext(v.toString())
    
    def doDefault(implicit state:SpaceState) = false
    
    override def renderInputXml(prop:Property[_,_], context:QLContext, currentValue:DisplayPropVal, v:ElemValue):NodeSeq = {
      if (get(v))
        <input type="checkbox" checked="checked" />
      else
        <input type="checkbox"/>
    }
  }
}
