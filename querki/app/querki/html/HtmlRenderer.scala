package querki.html

import scala.xml.{Attribute, NodeSeq, Null, Text, Xhtml}

import play.api.Logger
import play.api.data.Form
import play.api.templates.Html

import models._

import querki.ecology._

import querki.uservalues.TUserValue
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
class HtmlRendererEcot(e:Ecology) extends QuerkiEcot(e) with HtmlRenderer with querki.core.LinkUtils {
  
  lazy val Links = interface[querki.links.Links]
  lazy val Tags = interface[querki.tags.Tags]
  lazy val UserValues = interface[querki.uservalues.UserValues]
  
  lazy val InternalProp = Core.InternalProp
  lazy val NameType = Core.NameType
  lazy val NewTagSetType = Tags.NewTagSetType
  
  /*********************************
   * PUBLIC API
   *********************************/
  
  def renderPropertyInput(rc:RequestContext, prop:Property[_,_], currentValue:DisplayPropVal, specialization:Set[RenderSpecialization] = Set(Unspecialized)):Html = {
    val state = rc.state.get
    val cType = prop.cType
    val pType = prop.pType
    val rendered = doRender(cType, pType, rc, prop, currentValue, specialization)
    val xml3 = addEditorAttributes(rendered, currentValue, prop, currentValue.inputControlId)
    // TODO: this is *very* suspicious, but we need to find a solution. RenderTagSet is trying to pass JSON structures in the
    // value field, but for that to be JSON-legal, the attributes need to be single-quoted, and the strings in them double-quoted.
    // That isn't the way things come out here, so we're kludging, but I worry about potential security holes...
    val xmlFixedQuotes = Xhtml.toXhtml(xml3).replace("\'", "&#39;").replace("\"", "\'").replace("&quot;", "\"")
    Html(xmlFixedQuotes)
  }
  
//  def propValFromUser(prop:Property[_,_], str:String)(implicit state:SpaceState):QValue = {
//    handleSpecialized(prop, str).getOrElse(prop.cType.fromUser(str, prop, prop.pType))
//  }
  
  // TODO: refactor this with Collection.fromUser():
  def propValFromUser(fieldIds:FieldIds, on:Option[Thing], form:Form[_], context:QLContext):FormFieldInfo = {
    val prop = fieldIds.p
    implicit val s = context.state
    // TODO: this Ecot has a lot of incestuous tests of NameType. I'm leaving these ugly for now. Find a better solution,
    // like lifting out a more public trait to test against.
    if (prop.cType == QSet && (prop.pType.isInstanceOf[querki.core.IsNameType] || prop.pType == LinkType || prop.pType == NewTagSetType)) {
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

      withType(UserValues.getUserType(prop.pType).getOrElse(prop.pType))
    }
  }
  
  def renderThingDefault(thing:Thing)(implicit rc:RequestContext):Wikitext = {
    val listMap = thing.props.map { entry =>
      val propOpt = rc.state.get.prop(entry._1)
      propOpt match {
        case Some(prop) => {
          val pv = prop.pair(entry._2)
          "<dt>" + prop.displayName + "</dt><dd>" + pv.render(thing.thisAsContext).display + "</dd>"
        }
        case None => "<dt>" + entry._1 + "</dt><dd>Property not found!</dd>"
      }
    }
    HtmlWikitext(Html(listMap.mkString("<dl>", "", "</dl>")))    
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
      val asEditor = {
        // HACK: if this is an intermediate Model Property, we must *not* add propEditor, or we wind
        // up with that conflicting with the propEditor of the actual fields underneath it. There might
        // be a more general concept fighting to break out here, but I'm not sure.
        prop.pType match {
          case mt:querki.types.ModelTypeBase => addClasses(elem, "modelValue")
          case _ => addClasses(elem, "propEditor")
        }
      }
      val xml2 = asEditor.asInstanceOf[scala.xml.Elem] %
      	Attribute("name", Text(newName),
    	Attribute("data-prop", Text(prop.id.toThingId),
    	Attribute("data-propId", Text(currentValue.fullPropId),
    	Attribute("id", Text(id), Null))))
      val xml3 = currentValue.thingId match {
        case Some(thing) => xml2 % Attribute("data-thing", Text(thing), Null)
        case None => xml2
      }
      xml3
    }
  }
  
  def doRender(cType:Collection, pType:PType[_], rc:RequestContext, prop:Property[_,_], currentValue:DisplayPropVal, specialization:Set[RenderSpecialization]):NodeSeq = {
    renderSpecialized(cType, pType, rc, prop, currentValue, specialization).getOrElse(cType.renderInput(prop, rc, currentValue, pType))
  }
  
  def renderSpecialized(cType:Collection, pType:PType[_], rc:RequestContext, prop:Property[_,_], currentValue:DisplayPropVal, specialization:Set[RenderSpecialization]):Option[NodeSeq] = {
    // TODO: make this more data-driven. There should be a table of these.
    val state = rc.state.get
    val userType = UserValues.getUserType(pType)
    val userValueRendered = userType match {
      case Some(uvt) if (specialization.contains(FromEditFunction)) => {
        Some(doRender(cType, uvt, rc, prop, currentValue, specialization)) 
      }
      case _ => None
    }
    if (userValueRendered.isDefined)
      userValueRendered
    else if (cType == Optional && pType == YesNoType)
      Some(renderOptYesNo(state, prop, currentValue))
    else if (cType == Optional && pType == LinkType)
      Some(renderOptLink(rc, prop, currentValue))
    else if (cType == QSet && (pType == Core.NameType || pType == Tags.TagSetType || pType == LinkType || pType == NewTagSetType)) {
      if (specialization.contains(PickList))
        Some(renderPickList(state, prop, currentValue, specialization))
      else
        Some(renderTagSet(state, prop, currentValue))
    } else
      None
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
          <button type="button" class="radioBtn btn btn-primary active" value={value}>{label}</button>
        else
          <button type="button" class="radioBtn btn btn-primary" value={value}>{label}</button>
      }
      val id = currentValue.inputControlId + "-" + label
      val name = currentValue.inputControlId
      // Note that, to work for interactive controls, the special AJAX properties must be on the individual buttons!
      val onThing = currentValue.on.flatMap(_.asThing)
      addEditorAttributes(inputElem, currentValue, prop, id) // ++ <label for={id}>{label}</label>
    }
    
      <span class="btn-group" data-toggle="buttons-radio" name={currentValue.inputControlId + "-wrapper"}>{oneButton("Yes", "true", (isSet && v))}{oneButton("Maybe", "maybe", (!isSet))}{oneButton("No", "false", (isSet && !v))}</span>
  }
  
  def renderOptLink(rc:RequestContext, prop:Property[_,_], currentValue:DisplayPropVal):NodeSeq = {
    implicit val s = rc.state.get
    val pType = LinkType
    val pair = currentValue.effectiveV.map(propVal => if (propVal.cv.isEmpty) (false, pType.default) else (true, propVal.first)).getOrElse((false, pType.default))
    val isSet = pair._1
    val v = pType.get(pair._2)
    
    val results = <select class="_linkSelect"> 
      <option value={UnknownOID.id.toString}>Nothing selected</option>
      {
      renderInputXmlGuts(prop, rc, currentValue, ElemValue(v, LinkType))
    } </select>
    results
  }
  
  def getTagSetNames(state:SpaceState, prop:Property[_,_], currentValue:DisplayPropVal):Option[Iterable[(String,String)]] = {
    val currentV = currentValue.effectiveV
    val pt = prop.pType
    
    def getKeyAndVal(elem:ElemValue):(String, String) = {
      pt match {
        case linkType:querki.core.TypeCreation#LinkType => {
          val oid = linkType.get(elem)
          // TODO: cheating! This should go through LinkType.follow, but we don't have a Context yet:
          val tOpt = state.anything(oid)
          val name = tOpt.map(_.displayName).getOrElse(oid.toThingId.toString)
          (oid.toString, name)
        }
        case nameType:querki.core.IsNameType => {
          val name = nameType.get(elem)
          (name, name)          
        }
        case NewTagSetType => {
          val name = NewTagSetType.get(elem).text
          (name, name)                    
        }
        case _ => throw new Exception("renderTagSet got unexpected type " + pt)
      }
    }
    currentV.map(_.cv.map(getKeyAndVal(_)))    
  }
  
  def JSONescape(str:String):String = {
    str.replace("\\", "\\\\").replace("\"", "\\\"")
  }
  
  def renderTagSet(state:SpaceState, prop:Property[_,_], currentValue:DisplayPropVal):NodeSeq = {
    val rawList = getTagSetNames(state, prop, currentValue)
    
    // We treat names/tags and links a bit differently, although they look similar on the surface:
    val isNameType = prop.pType == Tags.TagSetType || prop.pType == NewTagSetType
    val current = "[" + rawList.map(_.map(keyVal => "{\"display\":\"" + JSONescape(keyVal._2) + "\", \"id\":\"" + JSONescape(keyVal._1) + "\"}").mkString(", ")).getOrElse("") + "]"
    <input class="_tagSetInput" data-isNames={isNameType.toString} type="text" data-current={current}></input>
  }
  
  /**
   * This is an alternate renderer for Tag/List Sets. It displays a list of *all* of the candidate Things
   * (defined by the Link Model), and lets you choose them by checkboxes. A primitive first cut, but useful.
   */
  def renderPickList(state:SpaceState, prop:Property[_,_], currentValue:DisplayPropVal, specialization:Set[RenderSpecialization]):NodeSeq = {
    implicit val s = state
    val instancesOpt = for (
        propAndVal <- prop.getPropOpt(Links.LinkModelProp);
        modelOID <- propAndVal.firstOpt
        )
      yield (modelOID, state.descendants(modelOID, false, true))
      
    instancesOpt match {
      case Some((modelOID, allInstances)) => {
        val sortedInstances = allInstances.toSeq.sortBy(_.displayName).zipWithIndex
        val rawList = getTagSetNames(state, prop, currentValue)
        val currentMap = rawList.map(_.toMap)
        val isNameType = prop.pType.isInstanceOf[querki.core.IsNameType]
        
        val listName = currentValue.inputControlId + "_values"
        
        def isListed(item:Thing):Boolean = {
          currentMap match {
            case Some(map) => {
              if (isNameType) {
                val name = for (
                    propAndVal <- item.getPropOpt(Core.NameProp);
                    name <- propAndVal.firstOpt
                      )
                  yield name
                name.map(map.contains(_)).getOrElse(false)
              } else {
                map.contains(item.id.toString)
              }
            }
            case None => false
          }
        }
    
        <form><ul class="_listContent"> {
          sortedInstances.map { pair =>
            val (instance, index) = pair
            <li>{
            if (isListed(instance))
              Seq(<input name={s"$listName[$index]"} value={instance.id.toString} type="checkbox" checked="checked"></input>, Text(" " + instance.displayName))
            else
              Seq(<input name={s"$listName[$index]"} value={instance.id.toString} type="checkbox"></input>, Text(" " + instance.displayName))
            }</li>
          }
        } </ul> {
          if (specialization.contains(WithAdd)) {
            <div class="input-append">
              <input type="text" class="_quickCreateProp" placeholder="New Item Name" data-model={modelOID.toString} data-propid={querki.basic.MOIDs.DisplayNameOID.toString} data-addtolist="true"></input>
              <button type="button" class="btn _quickCreate">Add</button>
            </div>
          }
        } </form>
      }
      // TODO: we need a better way to specify this warning
      case None => <p class="warning">Can't display a Pick List for a Set that doesn't have a Link Model!</p>
    }
  }
  
  def handleSpecialized(prop:Property[_,_], newVal:String)(implicit state:SpaceState):Option[QValue] = {
    if (prop.cType == Optional && prop.pType == YesNoType)
      Some(handleOptional(prop, newVal, YesNoType, (_ == "maybe")))
    else if (prop.cType == Optional && prop.pType == LinkType)
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
    else if (prop.cType == Optional && pType == LinkType)
      Some(handleOptionalForm(prop, newVal, LinkType, (OID(_) == UnknownOID)))
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