package querki.html

import scala.xml.{Attribute, NodeSeq, Null, Text, Xhtml}

import play.api.Logger
import play.api.data.Form

import models._

import querki.core.{EmptyOptionValue, QLText}
import querki.ecology._
import querki.globals._
import querki.util.XmlHelpers
import querki.values._

import RenderSpecialization._

object MOIDs extends EcotIds(26)

/**
 * This is the top level object that knows about HTML. All rendering of Things into HTML,
 * and interpretation of HTML forms, should pass through here.
 * 
 * TODO: we are a *long* ways from having this working right -- there's a ton of
 * refactoring to do. But it's a start.
 * 
 * TODO: there should be a trait called something like InputRenderer, which this derives from,
 * generalizing the concept of rendering and response.
 */
class HtmlRendererEcot(e:Ecology) extends QuerkiEcot(e) with HtmlRenderer with querki.core.NameUtils with querki.core.LinkUtils {
  
  lazy val Choices = interface[querki.datamodel.Choices]
  lazy val Links = interface[querki.links.Links]
  lazy val QL = interface[querki.ql.QL]
  lazy val Tags = interface[querki.tags.Tags]
  
  lazy val InternalProp = Core.InternalProp
  lazy val NameType = Core.NameType
  lazy val NewTagSetType = Tags.NewTagSetType
  
  /*********************************
   * PUBLIC API
   *********************************/
  
  def renderPropertyInputStr(context:QLContext, prop:Property[_,_], currentValue:DisplayPropVal, 
      specialization:Set[RenderSpecialization] = Set(Unspecialized)):Future[String] = 
  {
    val state = context.state
    val cType = prop.cType
    val pType = prop.pType
    for {
      rendered <- doRender(cType, pType, context, prop, currentValue, specialization)
      xml3 = addEditorAttributes(rendered, currentValue, prop, currentValue.inputControlId)
    }
      // TODO: this is *very* suspicious, but we need to find a solution. RenderTagSet is trying to pass JSON structures in the
      // value field, but for that to be JSON-legal, the attributes need to be single-quoted, and the strings in them double-quoted.
      // That isn't the way things come out here, so we're kludging, but I worry about potential security holes...
      yield Xhtml.toXhtml(xml3).replace("\'", "&#39;").replace("\"", "\'").replace("&quot;", "\"")    
  }
  
  def renderPropertyInput(context:QLContext, prop:Property[_,_], currentValue:DisplayPropVal, 
      specialization:Set[RenderSpecialization] = Set(Unspecialized)):Future[QHtml] = 
  {
    for {
      xmlFixedQuotes <- renderPropertyInputStr(context, prop, currentValue, specialization)
    }
      yield QHtml(xmlFixedQuotes)
  }
  
  // TODO: refactor this with Collection.fromUser():
  def propValFromUser(fieldIds:FieldIds, on:Option[Thing], form:Form[_], context:QLContext):FormFieldInfo = {
    val prop = fieldIds.p
    implicit val s = context.state
    // TODO: this Ecot has a lot of incestuous tests of NameType. I'm leaving these ugly for now. Find a better solution,
    // like lifting out a more public trait to test against.
    if (prop.cType == QSet 
        && (prop.pType.isInstanceOf[querki.core.IsNameType] 
            || prop.pType.isInstanceOf[querki.core.IsLinkType] 
            || prop.pType == NewTagSetType)) 
    {
      handleTagSet(fieldIds, on, form, context)
    } else {
      def withType(pType:PType[_]):FormFieldInfo = {
        val spec = for (
          formV <- form(fieldIds.inputControlId).value;
          specialized <- handleSpecializedForm(prop, pType, formV)
            )
          yield specialized
        spec.getOrElse(prop.cType.fromUser(on, form, prop, pType, fieldIds.container, context.state))
      }

      withType(prop.pType)
    }
  }
  
  /**
   * Simplified version, designed for the Client. Really no longer clear to me that this belongs in HtmlRenderer at this point.
   * Maybe this should be at a more abstract level, with the Form-specific bits kept here at the HTML level? Especially now that
   * this is being called from the client, and no longer has much to do with rendering.
   * 
   * Note also that fieldIds is vestigial here -- we should really pass in the Property that's being changed.
   */
  def propValFromUser(fieldIds:FieldIds, vs:List[String], context:QLContext):FormFieldInfo = {
    val prop = fieldIds.p
    val pType = prop.pType
    implicit val s = context.state
  
    val spec = for {
      v <- vs.headOption
      specialized <- handleSpecializedForm(prop, pType, v)
    }
      yield specialized
    spec.getOrElse(prop.cType.fromUser(fieldIds, vs, context.state))
  }
  
  // TODO: I don't love having this renderer level dependent on QL. Think about whether this is the right place
  // for this code.
  def renderThingDefault(thing:Thing)(implicit rc:RequestContext, state:SpaceState):Future[Wikitext] = {
    val isModel = thing.ifSet(Core.IsModelProp)(state)
    val guts = """
        |  _filter(_not(_prop -> Internal Property)) ->
        |  _filter(_not((_prop -> _is(Name._self)) | ((_prop -> _is(Link Name._self)) | (_prop -> _is(Is a Model._self))))) ->
        |  _sort -> 
        |  ""<dt>[[_prop]]</dt><dd>[[
        |    _if(_prop -> Property Type -> _is(Thing Type), 
        |        _val -> _rawVal -> _commas, 
        |        _val -> _rawVal)]]</dd>""
      """
    
    val instanceGuts = """
          |<dl>
          |[[+$thing
          |  Instance Properties -> _foreach(
          |    +$prop
          |    ""<dt>[[$prop._self]]</dt><dd>[[
          |      _if(Property Type -> _is(Thing Type),
          |        $thing -> $prop -> _commas,
          |        $thing -> $prop)]]</dd>""
          |)]]
          |</dl>
      """
    
    val text =
      if (isModel)
        s"""### Instance Properties
          |$instanceGuts
          |
          |### Model Properties
          |<dl>
          |[[+$$thing
          |  _foreachProperty -> _foreach(
          |    +$$propInfo
          |    _filter($$thing -> Instance Properties -> _not(_contains($$propInfo -> _prop))) ->
          |$guts 
          |)]]
          |</dl>
          |[[_QLButton(""Show Instances"", ql=_instances -> _sort -> _bulleted)]]""".stripMargin
      else 
        s"""
          |$instanceGuts
          |[[_QLButton(""Things that use [[Name]]"", ql=_allRefs -> _sort -> _bulleted)]]""".stripMargin
          
    QL.process(QLText(text), thing.thisAsContext, None, Some(thing))
  }
  
  /*********************************
   * INTERNALS
   *********************************/
  
  def addClasses(nodes:NodeSeq, addedClasses:String):NodeSeq = {
    XmlHelpers.mapElems(nodes) { elem =>
      val classAttr = elem.attribute("class").map(_ :+ Text(" " + addedClasses)).getOrElse(Text(addedClasses))
      elem % Attribute("class", classAttr, Null)      
    }
  }
  
  def addEditorAttributes(nodes:NodeSeq, currentValue:DisplayPropVal, prop:Property[_,_], id:String):NodeSeq = {
    XmlHelpers.mapElems(nodes) { elem =>
      // If there is already a name specified, leave it in place. This is occasionally *very* important, as
      // in renderOptionalYesNo -- that needs the usual name to be on the *buttons*, not on the wrapper:
      val newName = elem.attribute("name").map(_.head.text).getOrElse(currentValue.inputControlId)
      val newId = elem.attribute("id").map(_.head.text).getOrElse(id)
      val asEditor = {
        // HACK: if this is an intermediate Model Property, we must *not* add propEditor, or we wind
        // up with that conflicting with the propEditor of the actual fields underneath it. There might
        // be a more general concept fighting to break out here, but I'm not sure.
        prop.pType match {
          case mt:querki.types.ModelTypeBase => addClasses(elem, "modelValue")
          case _ => addClasses(elem, "propEditor")
        }
      }
      val collId = prop.cType.id.toThingId.toString
      val xml2 = asEditor.asInstanceOf[scala.xml.Elem] %
        Attribute("name", Text(newName),
      Attribute("data-prop", Text(prop.id.toThingId),
      Attribute("data-propId", Text(currentValue.fullPropId),
      Attribute("data-collId", Text(collId),
      Attribute("id", Text(newId), Null)))))
      val xml3 = currentValue.thingId match {
        case Some(thing) => xml2 % Attribute("data-thing", Text(thing), Null)
        case None => xml2
      }
      xml3
    }
  }
  
  def doRender(cType:Collection, pType:PType[_], context:QLContext, prop:Property[_,_], 
      currentValue:DisplayPropVal, specialization:Set[RenderSpecialization]):Future[NodeSeq] = 
  {
    renderSpecialized(cType, pType, context, prop, currentValue, specialization)
      .getOrElse(cType.renderInput(prop, context, currentValue, pType))
  }
  
  def renderSpecialized(cType:Collection, pType:PType[_], context:QLContext, prop:Property[_,_], 
      currentValue:DisplayPropVal, specialization:Set[RenderSpecialization]):Option[Future[NodeSeq]] = 
  {
    // TODO: make this more data-driven. There should be a table of these.
    implicit val state = context.state
    pType match {
      // If the Type wants to own the rendering, on its head be it:
      case renderingType:FullInputRendering => Some(fut(renderingType.renderInputFull(prop, context, currentValue)))
      case _ => {
        prop.getFirstOpt(Choices.ChooseFromPropProp) match {
          case Some(chooseFromProp) => {
            // This is a Choice Property, so the renderer is a selection among those choices:
            Some(renderChoice(context, prop, currentValue, chooseFromProp))
          }
          case None => {
            if (cType == Optional && pType == YesNoType)
              Some(fut(renderOptYesNo(state, prop, currentValue)))
            else if (cType == Optional && pType.isInstanceOf[querki.core.IsLinkType])
              Some(renderOptLink(context, prop, currentValue))
            else if (Tags.isTaggableProperty(prop)) {
              if (specialization.contains(PickList))
                Some(renderPickList(state, context.request, prop, currentValue, specialization))
              else
                Some(renderTagSet(state, context.request, prop, currentValue))
            } else
              None            
          }
        }
      }
    }
  }
  
  /**
   * This is called when we have a full-fledged Choice Property.
   * 
   * A Choice is a value, of any Type, that is drawn from a list of options that is specified in another Property.
   * The Choice Property must specify the Choose From Property meta-prop, which says what the other Property is.
   * It *may* specify the Choose From Thing meta-prop (which says which other Thing that other Property is found on),
   * or the Choose From Thing Through meta-prop (which points to *another* Property, which indicates which Thing to look
   * on). If neither is provided, the Choose From Property points to a list directly on the main Property; this is
   * expected to be the conventional case once we have a proper creation UI for these.
   */
  def renderChoice(context: QLContext, prop: AnyProp, currentValue: DisplayPropVal, chooseFromPropId: OID): Future[NodeSeq] = {
    implicit val state = context.state
    val targetType = prop.pType
    val isOptional = prop.cType == Optional
    
    def construct(
      chooseFromProp: AnyProp, 
      chooseFromOpt: Option[Thing], 
      chooseThroughOpt: Option[AnyProp], 
      currentSelectedElemOpt: Option[ElemValue]): Future[NodeSeq] = 
    {
      val currentExists = currentSelectedElemOpt.isDefined
      val optionsFut: Future[Iterable[(String, Boolean, String)]] = chooseFromOpt.map(_.getPropVal(chooseFromProp).elems) match {
        case Some(optionElems) => {
          // There is a current Thing that is providing the values, so get them from there:
          val optionFuts = optionElems.map { option =>
            val isSelected = currentSelectedElemOpt.map(cur => targetType.matches(cur, option)).getOrElse(false)
            // TODO: Eeeeevil! This shouldn't be hard-coded, but Links are sadly weird right now:
            val vStr =
              if (targetType == LinkType)
                option.get(LinkType).toString
              else
                targetType.toUser(option)
            targetType.wikify(context)(option).map { wiki =>
              (wiki.strip.toString, isSelected, vStr)
            }
          }
          Future.sequence(optionFuts)
        }
        // We don't have a current Thing, so there are no current options:
        case None => fut(Iterable.empty)
      }
      
      def unselectedOption: Seq[scala.xml.Elem] = {
        if (isOptional)
          Seq(<option value={EmptyOptionValue}>Nothing selected</option>)
        else
          Seq.empty
      }
      
      def invalidCurrentOption(invalidCurrentOpt: Option[String], chooseThrough: AnyProp): Seq[scala.xml.Elem] = {
        invalidCurrentOpt match {
          case Some(display) => Seq(<option disabled="disabled" selected="selected">{display} (invalid)</option>)
          case None => {
            if (chooseFromOpt.isDefined)
              Seq.empty
            else
              Seq(<option disabled="disabled" selected="selected">Choose from {chooseThrough.displayName}</option>)
          }
        }
      }
      
      def innards(options: Iterable[(String, Boolean, String)]) = {
        options.map { case (display, isSelected, v) =>
          if (isSelected)
            <option value={v} selected="selected">{display}</option>
          else
            <option value={v}>{display}</option>
        }        
      }
      
      optionsFut.flatMap { options =>
        val invalidCurrentFut: Future[Option[String]] =
          if (!currentExists || options.exists(_._2))
            fut(None)
          else
            futOpt(currentSelectedElemOpt.map { elem =>
              elem.pType.wikify(context)(elem).map(_.strip.toString)
            })
        
        invalidCurrentFut.map { invalidCurrent =>
          chooseThroughOpt match {
            case Some(chooseThrough) => {
              val fieldId = new FieldIds(currentValue.on, chooseThrough)
              <select class="_depends" data-dependson={fieldId.inputControlId}>
                {unselectedOption}
                {invalidCurrentOption(invalidCurrent, chooseThrough)}
                {innards(options)}
              </select> 
            }
            case None => <select>{innards(options)}</select>
          }
        }
      }
    }
    
    val resultOpt = for {
      chooseFromPropRaw <- state.prop(chooseFromPropId)
      // Make sure the types line up. For now the current Prop and the Choose From Prop must be
      // exactly the same PType. We might eventually loosen this to a *limited* degree, but
      // it's a helpful check.
      chooseFromProp <- chooseFromPropRaw.confirmType(targetType)
      // Note that, for now, this only works with single values. Multi-valued Properties are going
      // to need more thought, to line up "currentValue" correctly.
      currentSelectedElem = currentValue.effectiveV.flatMap(_.firstOpt)
      // It is intentionally optional to have a Choose From Thing, to allow us to set that
      // dynamically in the Client.
      chooseFromThingOpt = prop.firstOpt(Choices.ChooseFromThingProp).flatMap(id => state.anything(id))
      // Alternately, you can use Choose From Thing Through, which indirectly specifies which Thing the
      // choices are coming from
      chooseFromThroughOpt = prop.firstOpt(Choices.ChooseFromThingThroughProp).flatMap(id => state.prop(id))
    }
      yield {
        if (chooseFromThingOpt.isDefined) {
          // An explicit Thing contains the list of options:
          construct(chooseFromProp, chooseFromThingOpt, chooseFromThroughOpt, currentSelectedElem)
        } else if (chooseFromThroughOpt.isDefined) {
          // This is the tricky case: we are pointing to another Property, on this same Thing, that says
          // which Thing to draw the list from. This is how we manage Dependent Choices:
          val curThingOpt = for {
            chooseFromRaw <- chooseFromThroughOpt
            // At least for now, Choose From Through must specify a Thing Property:
            chooseFrom <- chooseFromRaw.confirmType(LinkType)
            bundle <- currentValue.on
            thingId <- bundle.getFirstOpt(chooseFrom)
            thing <- state.anything(thingId)
          }
            yield thing
            
          construct(chooseFromProp, curThingOpt, chooseFromThroughOpt, currentSelectedElem)
        } else {
          // If no Thing is specified, we expect to find the Choose From Prop on this Prop itself:
          construct(chooseFromProp, Some(prop), chooseFromThroughOpt, currentSelectedElem)
        }
      }
      
    // TODO: this should be an ordinary Warning, not an Exception! How do we make that work in this stack?
    resultOpt.getOrElse(throw new Exception(s"Trying to show editor for poorly-formed Choice!"))
  }
  
  def renderOptYesNo(state:SpaceState, prop:Property[_,_], currentValue:DisplayPropVal):NodeSeq = {
    implicit val s = state
    val pType = YesNoType
    val pair = currentValue.effectiveV.map(propVal => if (propVal.cv.isEmpty) (false, pType.default) else (true, propVal.first)).getOrElse((false, pType.default))
    val isSet = pair._1
    val v = pType.get(pair._2)
    
    def oneButton(label:String, value:String, checked:Boolean):NodeSeq = {
      def inputElem = {
        if (checked)
          <label class="btn btn-primary active radioBtn">
            <input type="radio" value={value} checked="checked"/>
            {label}
          </label>
        else
          <label class="btn btn-primary radioBtn">
            <input type="radio" value={value}/>
            {label}
          </label>
      }
      val id = currentValue.inputControlId + "-" + label
      val name = currentValue.inputControlId
      // Note that, to work for interactive controls, the special AJAX properties must be on the individual buttons!
      val onThing = currentValue.on.flatMap(_.asThing)
      addEditorAttributes(inputElem, currentValue, prop, id) // ++ <label for={id}>{label}</label>
    }
    
    // To make this work on conventional forms like the Advanced Editor, we need a shim to hold the chosen value, because
    // the buttons aren't submittable form elements. Hence the hidden field below: when we click on a button, that gets
    // changed:
      <span class="btn-group _optYesNo" data-toggle="buttons" name={currentValue.inputControlId + "-wrapper"} id={currentValue.inputControlId + "-wrapper"}>
        {oneButton("True", "true", (isSet && v))}{oneButton("Maybe", "maybe", (!isSet))}{oneButton("False", "false", (isSet && !v))}
        <input type="hidden" id={currentValue.inputControlId} name={currentValue.inputControlId}
          value={if (isSet) {v.toString} else "maybe"}
        />
      </span>
  }
  
  def renderOptLink(context:QLContext, prop:Property[_,_], currentValue:DisplayPropVal):Future[NodeSeq] = {
    implicit val s = context.state
    val pType = LinkType
    val pair = currentValue.effectiveV.map(propVal => if (propVal.cv.isEmpty) (false, pType.default) else (true, propVal.first)).getOrElse((false, pType.default))
    val isSet = pair._1
    val v = pType.get(pair._2)
    
    renderInputXmlGuts(prop, context, currentValue, ElemValue(v, LinkType), true)
  }
  
  def getTagSetNames(state:SpaceState, rc:RequestContext, prop:Property[_,_], currentValue:DisplayPropVal):Future[Seq[(String,String)]] = {
    val currentV = currentValue.effectiveV
    val pt = prop.pType
    
    def getKeyAndVal(elem:ElemValue):Future[(String, String)] = {
      pt match {
        case linkType:querki.core.IsLinkType => {
          val oid = LinkType.get(elem)
          // TODO: cheating! This should go through LinkType.follow, but we don't have a Context yet:
          state.anything(oid) match {
            // This intentionally uses unsafe, due to where we are using it.
            // TODO: in principle, safe and unsafe names should really be different types, so we
            // can make heads or tails of this stuff:
            case Some(t) => t.unsafeNameOrComputed(rc, state) map ((oid.toString, _))
            case None => Future.successful((oid.toString, "Unknown"))
          }
        }
        case nameType:querki.core.IsNameType => {
          val name = nameType.get(elem)
          Future.successful((name, name))          
        }
        case NewTagSetType => {
          val name = NewTagSetType.get(elem).text
          Future.successful((name, name))                    
        }
        case _ => throw new Exception("renderTagSet got unexpected type " + pt)
      }
    }
    currentV.map { realV =>
      val futs = realV.cv.toSeq.map(getKeyAndVal(_))
      Future.sequence(futs)
    }.getOrElse(Future.successful(Seq.empty))
  }
  
  def JSONescape(str:String):String = {
    str.replace("\\", "\\\\").replace("\"", "\\\"")
  }
  
  def renderTagSet(state:SpaceState, rc:RequestContext, prop:Property[_,_], currentValue:DisplayPropVal):Future[NodeSeq] = {
    getTagSetNames(state, rc, prop, currentValue) map { rawList =>
      // We treat names/tags and links a bit differently, although they look similar on the surface:
      val isNameType = prop.pType == Tags.TagSetType || prop.pType == NewTagSetType
      val current = "[" + rawList.map(keyVal => "{\"display\":\"" + JSONescape(keyVal._2) + "\", \"id\":\"" + JSONescape(keyVal._1) + "\"}").mkString(", ") + "]"
      <input class="_tagSetInput" data-isNames={isNameType.toString} type="text" data-current={current}></input>
    }
  }
  
  /**
   * This is an alternate renderer for Tag/List Sets. It displays a list of *all* of the candidate Things
   * (defined by the Link Model), and lets you choose them by checkboxes. A primitive first cut, but useful.
   */
  def renderPickList(state:SpaceState, rc:RequestContext, prop:Property[_,_], currentValue:DisplayPropVal, specialization:Set[RenderSpecialization]):Future[NodeSeq] = {
    implicit val s = state
    val modelOpt = for {
      propAndVal <- prop.getPropOpt(Links.LinkModelProp)
      modelOID <- propAndVal.firstOpt
    }
      yield modelOID
      
    modelOpt match {
      case Some(modelOID) => {
        for {
          namesAndThings <- Core.linkCandidates(state, Some(rc), prop)
          zipped = namesAndThings.zipWithIndex
          rawList <- getTagSetNames(state, rc, prop, currentValue)
        }
        yield {        
          val currentMap = rawList.toMap
          val isNameType = prop.pType.isInstanceOf[querki.core.IsNameType]
          
          val listName = currentValue.inputControlId + "_values"
          
          def isListed(item:Thing):Boolean = {
            if (currentMap.isEmpty)
              false
            else
              if (isNameType) {
                val name = for (
                    propAndVal <- item.getPropOpt(Core.NameProp);
                    name <- propAndVal.firstOpt
                      )
                  yield name
                name.map(currentMap.contains(_)).getOrElse(false)
              } else {
                currentMap.contains(item.id.toString)
              }
          }
      
          // TODO: this should really all be generated client-side once everyone's on the new Client; we
          // should just send a fairly abstract ul with the data:
          val deleteableClass =
            if (isNameType)
              ""
            else
              " _deleteable"
          <form class={s"_pickList$deleteableClass"}><ul class="_listContent"> {
            zipped.map { pair =>
              val ((name, instance), index) = pair
              <li>{
              if (isListed(instance))
                Seq(<input class="_pickOption" name={s"$listName[$index]"} value={instance.id.toThingId.toString} type="checkbox" checked="checked"></input>, 
                    Text(" "),
                    <div class="_pickName">{name}</div>)
              else
                Seq(<input class="_pickOption" name={s"$listName[$index]"} value={instance.id.toThingId.toString} type="checkbox"></input>, 
                    Text(" "),
                    <div class="_pickName">{name}</div>)
              }</li>
            }
          } </ul> {
            if (specialization.contains(WithAdd)) {
              <div class="input-append _quickCreator">
              <input type="text" class="_quickCreateProp" placeholder="New Item Name" data-model={modelOID.toThingId.toString} data-propid={querki.basic.MOIDs.DisplayNameOID.toThingId.toString} data-addtolist="true"></input>
              <button type="button" class="btn _quickCreate">Add</button>
            </div>
            }
          } </form>
        }
      }
      // TODO: we need a better way to specify this warning
      case None => Future.successful(<p class="warning">Can't display a Pick List for a Set that doesn't have Restrict to Model!</p>)
    }
  }
  
  def handleSpecialized(prop:Property[_,_], newVal:String)(implicit state:SpaceState):Option[QValue] = {
    if (prop.cType == Optional && prop.pType == YesNoType)
      Some(handleOptional(prop, newVal, YesNoType, (_ == "maybe")))
    else if (prop.cType == Optional && prop.pType.isInstanceOf[querki.core.IsLinkType])
      Some(handleOptional(prop, newVal, LinkType, (OID(_) == UnknownOID)))
    else
      None
  }
  
  def handleOptional(prop:Property[_,_], newVal:String, pType:PType[_], isEmpty:String => Boolean)(implicit state:SpaceState):QValue = {
    if (isEmpty(newVal))
      Optional.default(pType)
    else
      Optional(pType.fromUser(newVal))
  }
  
  // TODO: refactor this together with the above. It's going to require some fancy type math, though:
  def handleSpecializedForm(prop:Property[_,_], pType:PType[_], newVal:String)(implicit state:SpaceState):Option[FormFieldInfo] = {
    if (prop.cType == Optional && pType == YesNoType)
      Some(handleOptionalForm(prop, newVal, YesNoType, (_ == "maybe")))
    else if (prop.cType == Optional && pType.isInstanceOf[querki.core.IsLinkType])
      Some(handleOptionalForm(prop, newVal, LinkType, _ == EmptyOptionValue))
    else
      None
  }
  
  def handleOptionalForm(prop:Property[_,_], newVal:String, pType:PType[_], isNone:String => Boolean)(implicit state:SpaceState):FormFieldInfo = {
    if (isNone(newVal))
      // This is a bit subtle: there *is* a value, which is "None"
      FormFieldInfo(prop, Some(Core.QNone), false, true)
    else
      FormFieldInfo(prop, Some(Optional(pType.fromUser(newVal))), false, true)
  }
  
  // TODO: TagSets come from Manifest, and the end result is similar to QList.fromUser(). Figure out
  // how to refactor these together, if possible.
  def handleTagSet(fieldIds:FieldIds, on:Option[Thing], form:Form[_], context:QLContext):FormFieldInfo = {
    val prop = fieldIds.p
    implicit val s = context.state
    // TODO: this stuff testing for empty isn't really type-specific -- indeed, it is handling the button that is
    // rendered in editThing.html. So it probably belongs at a higher level?
    val empty = form(fieldIds.emptyControlId).value map (_.toBoolean) getOrElse false
    if (empty) {
      FormFieldInfo(prop, None, true, true)
    } else {
      val pt = prop.pType
      val oldListName = fieldIds.inputControlId + "_values"
      val oldList = form(oldListName)
      val oldIndexes = oldList.indexes
      val oldRaw =
        for (i <- oldIndexes;
             v <- oldList("[" + i + "]").value)
          yield v
      // TODO: some nasty abstraction breakage here. We shouldn't know that the internal is List:
      val oldVals = oldRaw.map(pt.fromUser(_)).toList
      FormFieldInfo(prop, Some(Core.makeSetValue(oldVals, pt, context)), false, true)
    }
  }  
}
