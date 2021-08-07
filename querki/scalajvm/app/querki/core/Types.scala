package querki.core

import scala.xml.NodeSeq

import models._

import querki.api.commonName
import querki.basic.PlainTextBaseType
import querki.ecology._
import querki.globals._
import querki.ql.QLPhrase
import querki.util.{PublicException, QLog}
import querki.values.{ElemValue, QFut, QLContext, QValue, RequestContext, ShowLinksAsFullAnchors, SpaceState}

import MOIDs._

/**
 * Trivial marker trait, that simply identifies the "Text Types" that are similarly serializable.
 */
trait IsTextType {

  /**
   * Fetch the stringified value of this text. Used for coercion. Text types should implement this.
   */
  def rawString(elem: ElemValue): String

  /**
   * Create an element of this type. Mainly intended to allow the Text types to coerce to each other.
   * Note that PType always has this, so you don't need to implement it separately.
   */
  def apply(raw: String): ElemValue
}

/**
 * Trivial marker trait, that identifies the "Link Types".
 */
trait IsLinkType

trait WithQL {
  def QL: querki.ql.QL
}

/**
 * Represents a Type that you can turn into a URL.
 */
trait URLableType {
  def getURL(context: QLContext)(v: ElemValue): Option[String]
  def getDisplay(context: QLContext)(v: ElemValue): Future[Option[String]]
}

/**
 * Trait to mix into a Property that has opinions about which Links should be presented as candidates
 * in the Editor.
 */
trait LinkCandidateProvider {

  def getLinkCandidates(
    state: SpaceState,
    currentValue: DisplayPropVal
  ): Seq[Thing]
}

trait TextTypeBasis { self: CoreEcot with WithQL =>

  trait TextTypeUtils { self: SystemType[_] with IsTextType =>

    lazy val ParsedTextType = QL.ParsedTextType

    def validateText(
      v: String,
      prop: Property[_, _],
      state: SpaceState
    ): Unit = {
      for {
        minLengthVal <- prop.getPropOpt(Types.MinTextLengthProp)(state)
        minLength <- minLengthVal.firstOpt
        if (v.trim().length() < minLength)
      } throw new PublicException("Types.Text.tooShort", prop.displayName, minLength)
    }

    // All Text Types (except errors) should be coerceable to each other:
    override def canCoerceTo(other: PType[_]): Boolean = {
      other match {
        case et: querki.values.IsErrorType => false
        case itt: IsTextType               => true
        // We can use Text as ParsedText (that is, Wikitext output). This is safe, because ParsedText
        // is assumed to be pre-rendering, and the output will get neutered if someone does something
        // nasty. Note that the reverse is *not* universally true, but so far we don't have any use
        // cases where we need to treat ParsedText as, eg, PlainText.
        case ParsedTextType => true
        case _              => false
      }
    }

    override def coerceTo(
      other: PType[_],
      elem: ElemValue
    ): ElemValue = {
      other match {
        case et: querki.values.IsErrorType =>
          throw new Exception(s"PType $displayName can not be coerced to ${other.displayName}!")
        case itt: IsTextType => {
          val str = rawString(elem)
          itt(str)
        }
        case ParsedTextType => {
          val str = rawString(elem)
          ParsedTextType(Wikitext(str))
        }
        case _ => throw new Exception(s"PType $displayName can not be coerced to ${other.displayName}!")
      }
    }
  }

  abstract class TextTypeBase(
    oid: OID,
    pf: PropMap
  ) extends SystemType[QLText](oid, pf)
       with PTypeBuilder[QLText, String]
       with querki.ql.CodeType
       with IsTextType
       with TextTypeUtils {
    import Collections.emptyTextMarkerStr

    private lazy val Core = interface[querki.core.Core]

    // NOTE: serialization and deserialization deal with a special "marker" character.
    // This is important, because otherwise a List/Set of Text, of length 1, where that single
    // element is empty, serializes to empty String -- the same as length 0. This turns out to
    // confuse Publication horribly, and is generally Wrong.
    def doDeserialize(v: String)(implicit state: SpaceState) = {
      if (v.equals(emptyTextMarkerStr))
        QLText("")
      else
        QLText(v)
    }

    def doSerialize(v: QLText)(implicit state: SpaceState) = {
      if (v.text.isEmpty)
        emptyTextMarkerStr
      else
        v.text
    }
    // We do *not* want toUser() to be doing the empty-text marker thing.
    override def doToUser(v: QLText)(implicit state: SpaceState): String = v.text

    def doWikify(
      context: QLContext
    )(
      v: QLText,
      displayOpt: Option[Wikitext] = None,
      lexicalThing: Option[PropertyBundle] = None
    ) = {
      QL.process(v, context, lexicalThing = lexicalThing)
    }
    def doDefault(implicit state: SpaceState) = QLText("")
    def wrap(raw: String): valType = QLText(raw)

    override def validate(
      v: String,
      prop: Property[_, _],
      state: SpaceState
    ): Unit = validateText(v, prop, state)

    override def qlApplyFromProp(
      inv: Invocation,
      prop: Property[QLText, _]
    ): Option[QFut] = {
      implicit val s = inv.state
      val result: QFut =
        for {
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
          processed <- inv.fut(QL.process(qlText, elemContext, Some(inv), Some(bundle), Some(prop)))
        }
        // ... and process that element through QL.
        yield Core.ExactlyOne(QL.ParsedTextType(processed))

      Some(result)
    }

    def code(elem: ElemValue): String = get(elem).text

    override def doToUrlParam(
      v: QLText,
      raw: Boolean
    )(implicit
      state: SpaceState
    ): String = {
      if (raw)
        s"""${querki.util.SafeUrl(v.text)}"""
      else
        s"""""${querki.util.SafeUrl(v.text)}"""""
    }

    def doComputeMemSize(v: QLText): Int = v.text.length

    def rawString(elem: ElemValue): String = {
      val v = get(elem)
      // TBD: does v need to go through the wikitext-processing machinery here?
      v.text
    }
  }
}

trait NameableType {
  def getName(context: QLContext)(v: ElemValue): String

  /**
   * Similar to getName, but if this Type potentially has multiple legitimate names, this
   * returns all of them. Always returns canonical form.
   */
  def getNames(context: QLContext)(v: ElemValue): Seq[String] = Seq(NameUtils.canonicalize(getName(context)(v)))
}

// Marker trait for NameTypeBase and everything that descends from it:
trait IsNameType extends PType[String] with IsTextType

trait NameTypeBasis { self: CoreEcot with NameUtils with WithQL =>

  /**
   * The Type for Display Names -- similar to Text, but not identical
   */
  abstract class NameTypeBase(
    tid: OID,
    pf: PropMap
  ) extends SystemType[String](tid, pf)
       with SimplePTypeBuilder[String]
       with NameableType
       with IsNameType {
    def doDeserialize(v: String)(implicit state: SpaceState) = toDisplay(v)
    def doSerialize(v: String)(implicit state: SpaceState) = toInternal(v)

    def doValidate(v: String): Unit = {
      if (v.length() == 0)
        throw new PublicException("Types.Name.empty")
      else if (v.length() > 254)
        throw new PublicException("Types.Name.tooLong")

      v match {
        case nameRegex(_*) => // As it should be
        case _             => throw new PublicException("Types.Name.badChars")
      }
    }

    override def validate(
      v: String,
      prop: Property[_, _],
      state: SpaceState
    ): Unit = {
      doValidate(v)
    }

    override def doToUser(v: String)(implicit state: SpaceState): String = toDisplay(v)

    override protected def doFromUser(v: String)(implicit state: SpaceState): String = {
      doValidate(v)
      toDisplay(v)
    }

    def rawString(elem: ElemValue): String = {
      get(elem)
    }

    def getName(context: QLContext)(v: ElemValue) = canonicalize(get(v))

    def nameToLink(
      context: QLContext
    )(
      v: String,
      displayOpt: Option[Wikitext] = None
    ) = {
      val display = displayOpt.getOrElse(Wikitext(v))
      Wikitext("[") + display + Wikitext("](" + toUrl(v) + ")")
    }

    override def doComp(
      context: QLContext
    )(
      left: String,
      right: String
    ): Boolean = compareNames(left, right)

    def doDefault(implicit state: SpaceState) = ""

    override def doMatches(
      left: String,
      right: String
    ): Boolean = equalNames(left, right)

    override def canCoerceTo(other: PType[_]): Boolean = {
      // Do *not* allow conversion to error; while it's technically a text type, semantically it's
      // quite different, and shouldn't be considered comparable.
      other != QL.ErrorTextType && (other.isInstanceOf[IsTextType] || other == QL.ParsedTextType)
    }

    override def coerceTo(
      other: PType[_],
      elem: ElemValue
    ): ElemValue = {
      if (other == QL.ParsedTextType)
        QL.ParsedTextType(Wikitext(get(elem)))
      else other match {
        case tt: IsTextType => new ElemValue(QLText(get(elem)), other)
        case _              => throw new Exception(s"Can not coerce NameTypeBase to ${other.displayName}")
      }
    }

    def doComputeMemSize(v: String): Int = v.length
  }
}

trait IntTypeBasis { self: CoreEcot =>
  import scala.math.Numeric._

  def ExactlyOne: Collection

  /**
   * The base Type for numbers
   */
  abstract class NumericTypeBase[T : Numeric](
    tid: OID,
    pf: PropMap
  ) extends SystemType[T](tid, pf)
       with SimplePTypeBuilder[T] {
    override val displayEmptyAsBlank: Boolean = true

    def fromStr(v: String): T
    def doDefault(implicit state: SpaceState): T
    def toT(i: Int): T
    type nType = T
    lazy val numeric = implicitly[Numeric[T]]

    def doDeserialize(v: String)(implicit state: SpaceState): T =
      try {
        fromStr(v)
      } catch {
        case ex: java.lang.NumberFormatException => throw new PublicException("Types.Int.badFormat")
      }

    def doSerialize(v: T)(implicit state: SpaceState) = v.toString

    def doWikify(
      context: QLContext
    )(
      v: T,
      displayOpt: Option[Wikitext] = None,
      lexicalThing: Option[PropertyBundle] = None
    ) =
      Future.successful(Wikitext(v.toString))

    override def validate(
      v: String,
      prop: Property[_, _],
      state: SpaceState
    ): Unit = {
      implicit val s = state
      if (v.length == 0)
        throw new PublicException("Types.Int.empty")
      for {
        minValPO <- prop.getPropOpt(Types.MinIntValueProp)(state)
        minVal <- minValPO.firstOpt
        if (numeric.lt(doDeserialize(v), toT(minVal)))
      } throw new PublicException("Types.Int.tooLow", prop.displayName, minVal)

      for {
        maxValPO <- prop.getPropOpt(Types.MaxIntValueProp)(state)
        maxVal <- maxValPO.firstOpt
        if (numeric.gt(doDeserialize(v), toT(maxVal)))
      } throw new PublicException("Types.Int.tooHigh", prop.displayName, maxVal)
    }

    override def doComp(
      context: QLContext
    )(
      left: T,
      right: T
    ): Boolean = { numeric.lt(left, right) }

    override def editorSpan(prop: Property[_, _]): Int = 1

    /**
     * TODO: eventually, we may want a more nuanced Int inputter. But this will do to start.
     */
  }

  class IntTypeBase(
    tid: OID,
    pf: PropMap
  ) extends NumericTypeBase[Int](tid, pf) {
    def fromStr(v: String) = v.toInt
    def doDefault(implicit state: SpaceState): Int = 0
    def toT(i: Int) = i
    def doComputeMemSize(v: Int): Int = 4
  }
}

trait LinkUtils { self: CoreEcot with NameUtils =>

  private lazy val Apps = interface[querki.apps.Apps]
  def InternalProp: Property[Boolean, Boolean]
  private lazy val Links = interface[querki.links.Links]

  def namePairs(
    candidates: Seq[Thing],
    state: SpaceState,
    rcOpt: Option[RequestContext]
  ): Future[Seq[(String, Thing)]] = {
    Future.sequence(candidates.map { t =>
      val name = rcOpt match {
        case Some(rc) => t.unsafeNameOrComputed(rc, state)
        case _        => fut(t.unsafeDisplayName)
      }
      name.map((_, t))
    })
  }

  /**
   * Given a Link Property, return all of the appropriate candidates for that property to point to.
   *
   * The Property passed into here should usually be of LinkType -- while in theory that's not required,
   * it would be surprising for it to be used otherwise.
   */
  def linkCandidates(
    state: SpaceState,
    rcOpt: Option[RequestContext],
    prop: Property[_, _]
  ): Future[Seq[(String, Thing)]] = {
    implicit val s = state

    val choiceOrderOpt =
      for {
        linkModelId <- prop.firstOpt(Links.LinkModelProp)
        linkModel <- state.anything(linkModelId)
        pv <- linkModel.getPropOpt(Links.ChoiceOrderProp)
        instanceIds = pv.rawList
      } yield instanceIds.map(state.anything(_)).flatten

    choiceOrderOpt match {
      // If the Model specifies a Choice Order, we just return that. Note that this can include results that
      // don't really make sense, but we give the user as much rope as ze wants:
      case Some(choiceOrder) => {
        namePairs(choiceOrder, state, rcOpt)
      }
      case None => {
        // We convert the candidates to a Set, to de-duplicate as we go up the App chain:
        val links =
          if (prop.hasProp(Links.LinkAllowAppsProp) && prop.first(Links.LinkAllowAppsProp))
            state.accumulateAll[Set[Thing]](linkCandidatesLocal(_, Links, prop), { (x, y) => x ++ y })
          else
            linkCandidatesLocal(state, Links, prop)

        val namePairsFut = namePairs(links.toSeq, state, rcOpt)

        // Since there isn't a Choice Order, sort by name:
        namePairsFut.map(_.sortBy(_._1))
      }
    }
  }

  /**
   * This enumerates all of the plausible candidates for the given property within this Space.
   *
   * This explicitly assumes that the Property does *not* point to a Model with Choice Order.
   */
  def linkCandidatesLocal(
    state: SpaceState,
    Links: querki.links.Links,
    prop: Property[_, _]
  ): Set[Thing] = {
    implicit val s = state

    // First, filter the candidates based on LinkKind:
    val allCandidatesIt =
      if (prop.hasProp(Links.LinkKindProp)) {
        val allowedKinds = prop.getPropVal(Links.LinkKindProp).cv
        def fetchKind(wrappedVal: ElemValue): Iterable[Thing] = {
          val kind = Links.LinkKindProp.pType.get(wrappedVal)
          kind match {
            case Kind.Thing      => state.things.values
            case Kind.Property   => state.spaceProps.values
            case Kind.Type       => state.types.values
            case Kind.Collection => state.colls.values
            case _               => Iterable.empty[Thing]
          }
        }
        (Iterable.empty[Thing] /: allowedKinds)((it, kind) => it ++: fetchKind(kind))
      } else {
        // No LinkKind specified, so figure that they only want Things:
        state.things.values
      }
    val allCandidates = allCandidatesIt.toList

    // Now, if they've specified a particular Model to be the limit of the candidate
    // tree -- essentially, they've specified what type you can link to -- filter for
    // that:
    val filteredByModel =
      if (prop.hasProp(Links.LinkModelProp)) {
        prop.firstOpt(Links.LinkModelProp) match {
          case Some(modelId) => {
            val allInstanceIds =
              state.descendants(modelId, true, true, true).map(_.id)
            allCandidates.filter(candidate => allInstanceIds.contains(candidate.id))
          }
          case None => allCandidates
        }
      } else {
        allCandidates
      }

    val filteredAsModel =
      if (prop.ifSet(Links.LinkToModelsOnlyProp)) {
        filteredByModel.filter(_.isModel)
      } else {
        filteredByModel
      }

    // Note that this used to filter out Shadows, supposedly to avoid duplication. That doesn't make any sense, though:
    // this list starts out as only including stuff from the local Space, not the App, so I'm not sure where the
    // duplications would come from, and that was introducing the bug that we weren't listing lots of options that
    // *should* be included.
    filteredAsModel.filterNot(_.ifSet(InternalProp)).toSet
  }

  def renderInputXmlGuts(
    prop: Property[_, _],
    context: QLContext,
    currentValue: DisplayPropVal,
    v: ElemValue,
    allowEmpty: Boolean
  ): Future[NodeSeq] = {
    val state = context.state
    // Give the Property a chance to chime in on which candidates belong here:
    val candidatesFut = prop match {
      case f: LinkCandidateProvider => {
        val rawCandidates = f.getLinkCandidates(state, currentValue)
        namePairs(rawCandidates, state, context.requestOpt)
      }
      case _ => linkCandidates(state, context.requestOpt, prop)
    }
    val realOptionsFut: Future[Seq[NodeSeq]] =
      candidatesFut.flatMap { candidates =>
        if (candidates.isEmpty) {
          Future.successful(Seq(<option value={EmptyOptionValue}><i>None defined</i></option>))
        } else {
          // Note: the unsafeDisplayNames below are because Scala's XML interpolator appears to be doing the
          // name sanitizing for us:
          val optFuts: Seq[Future[NodeSeq]] = candidates.map { candidatePair =>
            val (name, candidate) = candidatePair
            context.requestOpt.map(req => candidate.unsafeNameOrComputed(req, state)).getOrElse(
              Future.successful(candidate.unsafeDisplayName)
            ).map { name =>
              if (candidate.id == v.elem) {
                <option value={candidate.id.toString} selected="selected">{name}</option>
              } else {
                <option value={candidate.id.toString}>{name}</option>
              }
            }
          }
          Future.sequence(optFuts)
        }
      }

    val linkModel = prop.getPropOpt(Links.LinkModelProp)(state)

    for {
      realOptions <- realOptionsFut
      withOpt =
        // Note the second clause here: this is so that, even in a Required field, it will *initially* display
        // as "Nothing selected" until you choose a value.
        if (allowEmpty || (v.elem == UnknownOID))
          <option value={EmptyOptionValue}>Nothing selected</option> +: realOptions
        else
          realOptions
      fullContents = withOpt
      // TBD (QI.7w4g8u4): the whole "Create a New Thingy" option has been broken for *years* now,
      // and I've come to be suspicious of the concept. For now, we're going to just quietly remove
      // it:
//        linkModel match {
//        case Some(propAndVal) if (!propAndVal.isEmpty) => {
//          val model = state.anything(propAndVal.first).get
//          if (model.ifSet(Links.NoCreateThroughLinkProp)(state))
//            withOpt
//          else
//            withOpt :+ <option class="_createNewFromModel" data-model={model.toThingId} value={UnknownOID.id.toString}>Create a New {model.displayName}</option>
//        }
//        case _ => withOpt
//      }
    } yield <select class="_linkSelect">{fullContents}</select>
  }

  /**
   * The Type for Links to other Things
   */
  abstract class LinkTypeBase(
    tid: OID,
    pf: PropMap
  )(implicit
    e: Ecology
  ) extends SystemType[OID](tid, pf)(e)
       with SimplePTypeBuilder[OID]
       with NameableType
       with URLableType
       with IsLinkType {
    override def editorSpan(prop: Property[_, _]): Int = 6

    def doDeserialize(v: String)(implicit state: SpaceState) = OID(v)
    def doSerialize(v: OID)(implicit state: SpaceState) = v.toString

    def follow(context: QLContext)(v: OID) = context.state.anything(v)

    def followLink(context: QLContext): Option[Thing] = {
      // This only works if the valType is LinkType; otherwise, it will return None
      context.value.firstAs(this).flatMap(follow(context)(_))
    }

    def makeWikiLink(
      context: QLContext,
      thing: Thing,
      display: Wikitext
    ): Wikitext = {
      if (context.flags.contains(ShowLinksAsFullAnchors))
        // Rare case, intended for when we are rendering outside the context of the Space itself.
        // (Eg, in Notifications.)
        HtmlWikitext(s"""<a href="${Links.thingUrl(thing.id)(context.state)}">${display.strip.toString}</a>""")
      else
        // Normal case:
        Wikitext("[") + display + Wikitext("](" + thing.toThingId + ")")
    }

    def doWikify(
      context: QLContext
    )(
      v: OID,
      displayOpt: Option[Wikitext] = None,
      lexicalThing: Option[PropertyBundle] = None
    ) = {
      val target = follow(context)(v)
      target match {
        case Some(t) => {
          val displayFut = displayOpt match {
            case Some(display) => Future.successful(display)
            case None => {
              // TBD: the first version here is what it had been, which was working well but was inefficient
              // because it was injecting HtmlWikitext nodes all over the place. Does the second version
              // (which uses ordinary QWikitext, and is therefore *much* less overhead) work properly?
//              t.nameOrComputed(context.request, context.state).map(_.htmlWikitext)
              t.nameOrComputedWiki(context.request, context.state)
            }
          }
          displayFut.map(makeWikiLink(context, t, _))
        }
        case None => Future.successful(Wikitext("Bad Link: Thing " + v.toString + " not found"))
      }
    }

    override def doDebugRender(context: QLContext)(v: OID) = {
      val target = follow(context)(v)
      target match {
        case Some(t) => t.displayName + "(" + t.id.toThingId + ")"
        case None    => "???"
      }
    }

    def getNameFromId(context: QLContext)(id: OID) = {
      val tOpt = follow(context)(id)
      tOpt.map(thing => canonicalize(thing.displayName)).getOrElse(throw new Exception(
        "Trying to get name from unknown OID " + id
      ))
    }

    def getName(context: QLContext)(v: ElemValue) = {
      val id = get(v)
      getNameFromId(context)(id)
    }

    // Iff the display name and canonicalName don't match, return both of them:
    override def getNames(context: QLContext)(v: ElemValue) = {
      val id = get(v)
      follow(context)(id) match {
        case Some(thing) => {
          val disp = canonicalize(thing.displayName)
          thing.canonicalName.map(canonicalize) match {
            case Some(canon) if (canon != disp) => Seq(disp, canon)
            case _                              => Seq(disp)
          }
        }
        case None => throw new Exception("Trying to get name from unknown OID " + id)
      }
    }

    def getURL(context: QLContext)(elem: ElemValue): Option[String] = {
      for {
        v <- elem.getOpt(this)
        thing <- follow(context)(v)
      } yield thing.toThingId.toString()
    }

    def getDisplay(context: QLContext)(elem: ElemValue): Future[Option[String]] = {
      val optFut = for {
        v <- elem.getOpt(this)
        thing <- follow(context)(v)
      } yield thing.nameOrComputed(context.request, context.state).map(_.toString)

      futOpt(optFut)
    }

    // Links are sorted by their *display names*:
    override def doComp(
      context: QLContext
    )(
      left: OID,
      right: OID
    ): Boolean = {
      compareNames(getNameFromId(context)(left), getNameFromId(context)(right))
    }

    def doDefault(implicit state: SpaceState) = UnknownOID

    override def renderInputXml(
      prop: Property[_, _],
      context: QLContext,
      currentValue: DisplayPropVal,
      v: ElemValue
    ): Future[NodeSeq] = {
      renderInputXmlGuts(prop, context, currentValue, v, false)
    }

    override def doToUser(v: OID)(implicit state: SpaceState): String = {
      state.anything(v) match {
        case Some(thing) => thing.displayName
        case None        => v.toString
      }
    }

    override def doFromUser(str: String)(implicit state: SpaceState): OID = {
      if (str == EmptyOptionValue)
        // Legit: this signifies emptiness:
        UnknownOID
      // Is it an OID?
      else OID.parseOpt(str) match {
        case Some(oid) => oid
        case None => {
          // No -- is it a name?
          state.anythingByName(str) match {
            case Some(thing) => thing.id
            case None        => throw new Exception(s"Can't find a Thing named $str")
          }
        }
      }
    }

    override def doToUrlParam(
      v: OID,
      raw: Boolean
    )(implicit
      state: SpaceState
    ): String = v.toThingId

    def doComputeMemSize(v: OID): Int = 8
  }
}

trait CoreExtra {
  def Summary(text: String): (OID, QValue)
  def Details(text: String): (OID, QValue)
  def setInternal: (OID, QValue)
}

trait TypeCreation {
  self: CoreEcot
    with TextTypeBasis
    with NameTypeBasis
    with IntTypeBasis
    with LinkUtils
    with NameUtils
    with PlainTextBaseType
    with WithQL
    with Core
    with CoreExtra =>

  /**
   * Marker type, used to signify "no real type" in empty collections.
   */
  class UnknownType extends PType[Unit](UnknownOID, UnknownOID, UnknownOID, toProps(setName("Unknown Type"))) {
    def doDeserialize(v: String)(implicit state: SpaceState) = throw new Exception("Trying to use UnknownType!")
    def doSerialize(v: Unit)(implicit state: SpaceState) = throw new Exception("Trying to use UnknownType!")

    def doWikify(
      context: QLContext
    )(
      v: Unit,
      displayOpt: Option[Wikitext] = None,
      lexicalThing: Option[PropertyBundle] = None
    ) = throw new Exception("Trying to use UnknownType!")

    def renderInputXml(
      prop: Property[_, _],
      context: QLContext,
      currentValue: DisplayPropVal,
      v: ElemValue
    ): Future[NodeSeq] =
      throw new Exception("Trying to use UnknownType!")

    def doDefault(implicit state: SpaceState) = throw new Exception("Trying to use UnknownType!")
    def doComputeMemSize(v: Unit): Int = 0
  }

  /**
   * The Root Type, that all others are based on.
   *
   * Note that this was a late addition to the system, and is necessary mostly for Model Types to hang off of.
   */
  class UrType
    extends PType[Unit](
      UrTypeOID,
      SystemIds.systemOID,
      querki.core.MOIDs.RootOID,
      toProps(
        setName("Root Type"),
        (querki.conventions.MOIDs.PropSummaryOID ->
          ExactlyOne(ElemValue(
            QLText(
              """[[_modelForType -> _if(_isNonEmpty, ""Based on Model ____"", ""The Ur-Type, from which all others descend."")]]"""
            ),
            new DelegatingType(TextType)
          ))),
        (querki.conventions.MOIDs.PropDetailsOID ->
          ExactlyOne(ElemValue(QLText("""""".stripMargin), new DelegatingType(LargeTextType)))),
        (querki.editing.MOIDs.PreferredCollectionOID ->
          ExactlyOne(ElemValue(querki.core.MOIDs.OptionalOID, new DelegatingType(LinkType)))),
        (querki.basic.MOIDs.DisplayTextOID -> Optional(
          LargeTextType("""[[Summary -> ""**____**""]]
                          |
                          |[[Details]]
                          |
                          |### Properties that are based on [[Link Name]]
                          |
                          |[[_propsOfType -> _sort -> _bulleted]]""".stripMargin)
        )),
        setInternal
      )
    ) {
    def doDeserialize(v: String)(implicit state: SpaceState) = throw new Exception("Trying to use UrType!")
    def doSerialize(v: Unit)(implicit state: SpaceState) = throw new Exception("Trying to use UrType!")

    def doWikify(
      context: QLContext
    )(
      v: Unit,
      displayOpt: Option[Wikitext] = None,
      lexicalThing: Option[PropertyBundle] = None
    ) = throw new Exception("Trying to use UrType!")

    def renderInputXml(
      prop: Property[_, _],
      context: QLContext,
      currentValue: DisplayPropVal,
      v: ElemValue
    ): Future[NodeSeq] =
      throw new Exception("Trying to use UrType!")

    def doDefault(implicit state: SpaceState) = throw new Exception("Trying to use UrType!")
    def doComputeMemSize(v: Unit): Int = 0
  }

  class InternalMethodType
    extends SystemType[String](
      InternalMethodOID,
      toProps(
        setName("Internal Method Type"),
        setInternal,
        Summary("A system-created Function"),
        Details(
          """A system-created Function. You can not create these, and generally shouldn't worry about this Type.""".stripMargin
        )
      )
    )
       with SimplePTypeBuilder[String] {
    def boom = throw new Exception("InternalMethodType cannot be used conventionally. It simply wraps code.")
    def doDeserialize(v: String)(implicit state: SpaceState) = boom
    def doSerialize(v: String)(implicit state: SpaceState) = boom

    def doWikify(
      context: QLContext
    )(
      v: String,
      displayOpt: Option[Wikitext] = None,
      lexicalThing: Option[PropertyBundle] = None
    ) =
      Future.successful(Wikitext("Internal Method"))

    def doDefault(implicit state: SpaceState) = ""
    override def wrap(raw: String): valType = boom
    def doComputeMemSize(v: String): Int = 0
  }

  // TODO: this is essentially identical to Tag, isn't it? Is this even useful any more?
  class NameType
    extends NameTypeBase(
      NameTypeOID,
      toProps(
        setName("Link Name Type"),
        setInternal,
        Summary("The type for Link Names"),
        Details(
          """A "Link Name" is exactly that -- a name that *can* be applied to a Thing. It does not
            |necessarily mean a name that is currently in use: the Link Name can be for a Thing that already
            |exists, or it can simply be a Link Name with no actual Thing named by it yet.
            |
            |Link Names are very restricted: they can contain only letter, numbers, spaces, dashes and underscores.
            |User-defined names may not start with underscore. (System-defined names often do.)
            |
            |Any given Space may contain at most one Thing with any given Link Name -- you can't duplicate Link Names.
            |
            |When you choose "Link Name Type", the system will allow you to "Restrict to Model". This is optional, but sometimes
            |helpful. When you are editing a Link Name Property, the system will prompt you with existing Link Names. If you
            |choose a Model at creation time, it will only prompt you with Link Names of Instances of that Model. So if
            |you know what sorts of Things you will be naming in this Property, it is worth specifying that Model here.
            |
            |Note that Link Names are different from the more-common Names, which you will usually use. Normal Names
            |have far fewer restrictions, and are usually what you will see in practice. Most of the time, a Thing's
            |Link Name is derived automatically from its Name, by stripping out the illegal characters.
            |
            |Link Names are used mainly to generate the URLs for each Thing.
            |
            |Link Names are fairly advanced -- most users usually won't want to create a Link Name Property.""".stripMargin
        )
      )
    ) {
    override def editorSpan(prop: Property[_, _]): Int = 3

    def doWikify(
      context: QLContext
    )(
      v: String,
      displayOpt: Option[Wikitext] = None,
      lexicalThing: Option[PropertyBundle] = None
    ) =
      Future.successful(Wikitext(toDisplay(v)))
  }

  /**
   * The Type for Text -- probably the most common type in Querki
   */
  class TextType
    extends TextTypeBase(
      TextTypeOID,
      toProps(
        setName("Text Type"),
        (querki.conventions.MOIDs.PropSummaryOID ->
          ExactlyOne(ElemValue(
            QLText("A single line of text, which may contain QL expressions"),
            new DelegatingType(TextType)
          ))),
        (querki.conventions.MOIDs.PropDetailsOID ->
          ExactlyOne(ElemValue(
            QLText(
              """Text Type is almost the same as Large Text Type -- see Large Text Type for most of the details.
                |
                |The only real difference is that the input field for a Text Type Property is only a single line, and
                |Text Properties are usually intended to be relatively short. As a rule of thumb, if you can imagine
                |this Property ever holding more than a paragraph, use Large Text instead.""".stripMargin
            ),
            new DelegatingType(LargeTextType)
          )))
      )
    )
       with PTypeBuilder[QLText, String] {
    override def editorSpan(prop: Property[_, _]): Int = 12
  }

  /**
   * The Type for Large Text -- stuff that we expect to take up more space on-screen
   */
  class LargeTextType
    extends TextTypeBase(
      LargeTextTypeOID,
      toProps(
        setName("Large Text Type"),
        (querki.conventions.MOIDs.PropSummaryOID ->
          ExactlyOne(ElemValue(
            QLText("A block of text, which may contain QL expressions"),
            new DelegatingType(TextType)
          ))),
        (querki.conventions.MOIDs.PropDetailsOID ->
          ExactlyOne(ElemValue(
            QLText("""Large Text is one of the central Types in Querki: it is an arbitrarily long block of text,
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
                     |slightly different from its normal Name. If you're not sure, look in the small subtitle line on
                     |that page -- it should give the "Link Name", which is what you should use.
                     |
                     |Second -- Querki is all about Things with Properties. Say that you want to show the value of the Property
                     |named "Details" on your Thing. You would just say:
                     |```
                     |\[[Details\]]
                     |```
                     |That's it -- Querki is smart enough to know that, since you're naming a Property, it should just insert the
                     |value of that Property here.""".stripMargin),
            new DelegatingType(LargeTextType)
          )))
      )
    )
       with PTypeBuilder[QLText, String] {
    override def editorSpan(prop: Property[_, _]): Int = 12

    override def renderInputXml(
      prop: Property[_, _],
      context: QLContext,
      currentValue: DisplayPropVal,
      v: ElemValue
    ): Future[NodeSeq] =
      Future.successful(renderLargeText(prop, context, currentValue, v, this))
  }

  class LinkType
    extends LinkTypeBase(
      LinkTypeOID,
      toProps(
        setName(commonName(_.core.linkType)),
        Summary("A specific Thing"),
        Details("""This contains a single Thing in this Space. It can be any Thing: an Instance, a Model,
                  |a Property -- even a Type or the Space itself.
                  |
                  |When you create or edit a Property of `Thing Type`, the system will let you
                  |`Restrict to Model`. This is optional, but usually
                  |helpful. When you are editing a Thing Property, the system will prompt you with existing Things. If you
                  |choose a Model to restrict to, it will only prompt you with existing Instances of that Model. So if
                  |you know what sorts of Things you will be using in this Property, it is worth restricting to that Model.            
                  |""".stripMargin)
      )
    )

  /**
   * The Type for integers
   */
  class IntType
    extends IntTypeBase(
      IntTypeOID,
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
      )
    )

  class LongType
    extends NumericTypeBase[Long](
      LongTypeOID,
      toProps(
        setName("Large Number Type"),
        Summary("A number that can potentially be extremely big"),
        Details(
          """This is very similar to Whole Number Type, but can hold bigger numbers.
            |Whole Number Type can handle numbers up to 2,147,483,647, which is plenty for most
            |purposes. If you need more than that, use Large Number instead.
            |
            |(Technically, Large Number is what is called a "long integer", and can handle numbers
            |up to 9,223,372,036,854,775,807. It is slightly slower than Whole Number Type, so Querki uses Whole Number
            |for all of its built-in properties, but this slowdown shouldn't usually matter to you.)""".stripMargin
        )
      )
    ) {
    def fromStr(v: String) = v.toLong
    def doDefault(implicit state: SpaceState): Long = 0L
    def toT(i: Int) = i.toInt
    def doComputeMemSize(v: Long): Int = 8
  }

  class FloatType
    extends NumericTypeBase[Double](
      FloatTypeOID,
      toProps(
        setName("Floating Point Type"),
        Summary("A number with a decimal point"),
        Details("""Most numbers in Querki are integers, and you should use Whole Number Type for those.
                  |But sometimes, you need to describe something fractional, like 3.14159. For those, you use
                  |"floating point" -- a number with a decimal point, with as many digits of precision after the
                  |decimal point as you need.
                  |
                  |Technically, this is what is often called a "double" -- a 64-bit floating point number, with
                  |52 bits (about 16 decimal digits) in the mantissa and 11 bits for the exponent. This allows
                  |a good balance between pretty high precision and very large or small numbers.
                  |
                  |Note that floating-point numbers are subject to rounding errors. We will probably add special-purpose
                  |types for cases where this must not happen. (For example, Currency.) If you particularly
                  |need this, drop us a note!""".stripMargin)
      )
    ) {
    def fromStr(v: String) = v.toDouble
    def doDefault(implicit state: SpaceState): Double = 0
    def toT(i: Int) = i.toDouble
    def doComputeMemSize(v: Double): Int = 8

    override def doWikify(
      context: QLContext
    )(
      v: Double,
      displayOpt: Option[Wikitext] = None,
      lexicalThing: Option[PropertyBundle] = None
    ) = {
      displayOpt match {
        case Some(display) => {
          val format = s"%${display.raw.str}f"
          val stringified = format.format(v)
          fut(Wikitext(stringified))
        }
        case None => fut(Wikitext(v.toString))
      }
    }
  }

  /**
   * The TrueOrFalse Type -- or Boolean, as us geeks think of it. Originally named YesNo Type; the
   * code hasn't caught up to the present day yet.
   */
  class YesNoType
    extends SystemType[Boolean](
      YesNoTypeOID,
      toProps(
        setName("TrueOrFalse Type"),
        Summary("A true/false value"),
        Details("""A TrueOrFalse Property, as the name implies, allows you to say whether something is true or false.
                  |
                  |If you say that your Property is Optional TrueOrFalse, that essentially introduces the
                  |concept of "Maybe" -- a value that is neither True nor False.
                  |
                  |TrueOrFalse values are called "Boolean" in most programming languages; we are deliberately
                  |avoiding that particular bit of computer-science jargon.""".stripMargin)
      )
    )
       with SimplePTypeBuilder[Boolean] {

    // This is horribly hackish, and illustrates that editorSpan is more than just a function of
    // the PType:
    override def editorSpan(prop: Property[_, _]): Int = {
      if (prop.cType == Optional)
        2
      else
        1
    }

    // It turns out that Java's parseBoolean is both too tolerant of nonsense, and
    // doesn't handle many common cases. So we'll do it ourselves:
    def doDeserialize(v: String)(implicit state: SpaceState) = {
      v.toLowerCase() match {
        case "true"  => true
        case "false" => false

        case "1" => true
        case "0" => false

        case "yes" => true
        case "no"  => false

        case "on"  => true
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

        case _ => throw new Exception("I can't interpret " + v + " as a TrueOrFalse value")
      }
    }
    def doSerialize(v: Boolean)(implicit state: SpaceState) = v.toString

    def doWikify(
      context: QLContext
    )(
      v: Boolean,
      displayOpt: Option[Wikitext] = None,
      lexicalThing: Option[PropertyBundle] = None
    ) =
      Future.successful(Wikitext(v.toString()))

    def doDefault(implicit state: SpaceState) = false

    override def renderInputXml(
      prop: Property[_, _],
      context: QLContext,
      currentValue: DisplayPropVal,
      v: ElemValue
    ): Future[NodeSeq] = {
      Future.successful(if (get(v))
        <input type="checkbox" checked="checked" />
      else
        <input type="checkbox"/>)
    }

    def doComputeMemSize(v: Boolean): Int = 1
  }

  class TagType
    extends PlainTextType(
      querki.tags.MOIDs.NewTagSetOID,
      toProps(
        setName(commonName(_.core.tagType)),
        (querki.editing.MOIDs.PreferredCollectionOID ->
          ExactlyOne(ElemValue(querki.core.MOIDs.QSetOID, new DelegatingType(LinkType)))),
        Summary("A collection of arbitrary Tags that apply to this Thing"),
        Details(
          """A Tag Set is a way to add a bunch of "tags" to a Thing. It is typically used to
            |list the characteristics of this Thing.
            |
            |Note that a Tag is, technically just a Plain Text value, and can contain almost anything.
            |However, if it matches the Name or Link Name of a Thing, it will generally display as a link to that
            |Thing if you simply show it, and if it doesn't match a Thing, clicking on it will allow you to
            |create a Thing by that name. This way, you can add additional details about what this Tag means.
            |
            |It is strongly recommended that you create Sets of Tags -- there is special UI support for this,
            |and we've found that Sets of Tags are one of the most useful sorts of data in Querki. But you
            |are not required to do so -- in particular, if you really only want one value here, create an
            |Optional Tag.
            |
            |When you select Tag Type, the Editor will ask you if you want to Restrict to a Model. This is optional,
            |but can be very useful -- if it is set, it restricts the Tags that get offered to you when you are
            |doing data entry. If this Property has any sort of meaning -- if the Tag isn't completely open-ended
            |and arbitrary -- consider first creating a Model (which doesn't need anything more than a Name),
            |and using it in Restrict to Model for the Property. That will help keep your Tags better-organized.""".stripMargin
        )
      )
    ) {
    override def editorSpan(prop: Property[_, _]): Int = 12

    override def renderInputXml(
      prop: Property[_, _],
      context: QLContext,
      currentValue: DisplayPropVal,
      v: ElemValue
    ): Future[NodeSeq] = {
      fut(renderAnyText(prop, context, currentValue, v, this) { cv =>
        <input type="text" class="_tagInput" data-isnames="true" value={cv}/>
      })
    }

    override def doWikify(
      context: QLContext
    )(
      v: querki.basic.PlainText,
      displayOpt: Option[Wikitext] = None,
      lexicalThing: Option[PropertyBundle] = None
    ) = {
      val display = displayOpt.getOrElse(Wikitext(querki.util.HtmlEscape.escapeAll(v.text)))
      // NOTE: yes, there is danger of Javascript injection here. We deal with that at the QText layer,
      // since that danger is there in ordinary QText as well.
      Future.successful(Wikitext("[") + display + Wikitext(s"](${querki.util.SafeUrl(v.text)})"))
    }

    override def renderProperty(
      prop: Property[_, _]
    )(implicit
      request: RequestContext,
      state: SpaceState
    ): Option[Future[Wikitext]] = {
      Some(QL.process(
        QLText("""These tags are currently being used:
                 |[[_tagsForProperty -> _sort -> _bulleted]]""".stripMargin),
        prop.thisAsContext
      ))
    }
  }
}
