package querki.html

import scala.xml._

import play.api.Logger
import play.api.data.Form
import play.api.templates.Html

import models._
import models.system._

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
object HtmlRenderer {
  
  /*********************************
   * PUBLIC API
   *********************************/
  
  def renderPropertyInput(state:SpaceState, prop:Property[_,_], currentValue:DisplayPropVal):Html = {
    val cType = prop.cType
    val pType = prop.pType
    val rendered = renderSpecialized(cType, pType, state, prop, currentValue).getOrElse(cType.renderInput(prop, state, currentValue, pType))
    val xml3 = addEditorAttributes(rendered, currentValue.inputControlId, prop.id.toThingId, currentValue.inputControlId, currentValue.on.map(_.id.toThingId))
    Html(xml3.toString)
  }
  
  def propValFromUser(prop:Property[_,_], str:String):PropValue = {
    handleSpecialized(prop, str).getOrElse(prop.cType.fromUser(str, prop, prop.pType))
  }
  
  // TODO: refactor this with Collection.fromUser():
  def propValFromUser(prop:Property[_,_], on:Option[Thing], form:Form[_]):FormFieldInfo = {
    if (prop.pType == TagSetType) {
      handleTagSet(prop, on, form)
    } else {
      val fieldIds = FieldIds(on, prop)
      val spec = for (
        formV <- form(fieldIds.inputControlId).value;
        specialized <- handleSpecializedForm(prop, formV)
          )
        yield specialized
      spec.getOrElse(prop.cType.fromUser(on, form, prop, prop.pType))
    }
  }
  
  /*********************************
   * INTERNALS
   *********************************/
  
  def addClasses(elem:Elem, addedClasses:String):Elem = {
    val classAttr = elem.attribute("class").map(_ :+ Text(" " + addedClasses)).getOrElse(Text(addedClasses))
    elem % Attribute("class", classAttr, Null)
  }
  
  def addEditorAttributes(elem:Elem, name:String, prop:String, id:String, thingOpt:Option[String]):Elem = {
    val xml2 = addClasses(elem, "propEditor") %
    	Attribute("name", Text(name),
    	Attribute("data-prop", Text(prop),
    	Attribute("id", Text(id), Null)))
    val xml3 = thingOpt match {
      case Some(thing) => xml2 % Attribute("data-thing", Text(thing), Null)
      case None => xml2
    }
    xml3
  }
  
  def renderSpecialized(cType:Collection, pType:PType[_], state:SpaceState, prop:Property[_,_], currentValue:DisplayPropVal):Option[Elem] = {
    // TODO: make this more data-driven. There should be a table of these.
    if (cType == Optional && pType == YesNoType)
      Some(renderOptYesNo(state, prop, currentValue))
    else if (cType == Optional && pType == LinkType)
      Some(renderOptLink(state, prop, currentValue))
    else if (pType == TagSetType)
      Some(renderTagSet(state, prop, currentValue))
    else
      None
  }
  
  def renderOptYesNo(state:SpaceState, prop:Property[_,_], currentValue:DisplayPropVal):Elem = {
    val pType = YesNoType
    val pair = currentValue.v.map(propVal => if (propVal.cv.isEmpty) (false, pType.default) else (true, propVal.first)).getOrElse((false, pType.default))
    val isSet = pair._1
    val v = pType.get(pair._2)
    
    def oneButton(label:String, value:String, checked:Boolean):NodeSeq = {
      def inputElem = {
        if (checked)
          <input type="radio" checked="checked" value={value} />
        else
          <input type="radio" value={value} />        
      }
      val id = currentValue.inputControlId + "-" + label
      val name = currentValue.inputControlId + "-buttons"
      // Note that, to work for interactive controls, the special AJAX properties must be on the individual buttons!
      addEditorAttributes(inputElem, name, prop.id.toThingId, id, currentValue.on.map(_.id.toThingId)) ++ <label for={id}>{label}</label>
    }
    
      <span class="buttonset">{oneButton("Yes", "true", (isSet && v))}{oneButton("Maybe", "maybe", (!isSet))}{oneButton("No", "false", (isSet && !v))}</span>
  }
  
  def renderOptLink(state:SpaceState, prop:Property[_,_], currentValue:DisplayPropVal):Elem = {
    val pType = LinkType
    val pair = currentValue.v.map(propVal => if (propVal.cv.isEmpty) (false, pType.default) else (true, propVal.first)).getOrElse((false, pType.default))
    val isSet = pair._1
    val v = pType.get(pair._2)
    
    val results = <select class="_linkSelect"> 
      <option value={UnknownOID.id.toString}>Nothing selected</option>
      {
      LinkType.renderInputXmlGuts(prop, state, currentValue, ElemValue(v))
    } </select>
    results
  }
  
  def renderTagSet(state:SpaceState, prop:Property[_,_], currentValue:DisplayPropVal):Elem = {
    val currentV = currentValue.v
    val rawList:Option[List[String]] = currentV.map(_.rawList(TagSetType))
    val current = rawList.map(_.mkString(", ")).getOrElse("")
    <input class="_tagSetInput" data-propid={prop.toThingId} type="text" value={current}></input>
  }
  
  def handleSpecialized(prop:Property[_,_], newVal:String):Option[PropValue] = {
    if (prop.cType == Optional && prop.pType == YesNoType)
      Some(handleOptional(prop, newVal, YesNoType, (_ == "maybe")))
    else if (prop.cType == Optional && prop.pType == LinkType)
      Some(handleOptional(prop, newVal, LinkType, (OID(_) == UnknownOID)))
    else
      None
  }
  
  def handleOptional(prop:Property[_,_], newVal:String, pType:PType[_], isEmpty:String => Boolean):PropValue = {
    if (isEmpty(newVal))
      Optional.default(pType)
    else
      Optional(pType.fromUser(newVal))
  }
  
  // TODO: refactor this together with the above. It's going to require some fancy type math, though:
  def handleSpecializedForm(prop:Property[_,_], newVal:String):Option[FormFieldInfo] = {
    if (prop.cType == Optional && prop.pType == YesNoType)
      Some(handleOptionalForm(prop, newVal, YesNoType, (_ == "maybe")))
    else if (prop.cType == Optional && prop.pType == LinkType)
      Some(handleOptionalForm(prop, newVal, LinkType, (OID(_) == UnknownOID)))
    else
      None
  }
  
  def handleOptionalForm(prop:Property[_,_], newVal:String, pType:PType[_], isNone:String => Boolean):FormFieldInfo = {
    if (isNone(newVal))
      // This is a bit subtle: there *is* a value, which is "None"
      FormFieldInfo(prop, Some(Optional.None), false, true)
    else
      FormFieldInfo(prop, Some(Optional(pType.fromUser(newVal))), false, true)
  }
  
  // TODO: TagSets come from Manifest, and the end result is similar to QList.fromUser(). Figure out
  // how to refactor these together, if possible.
  def handleTagSet(prop:Property[_,_], on:Option[Thing], form:Form[_]):FormFieldInfo = {
    val fieldIds = FieldIds(on, prop)
    val oldListName = fieldIds.inputControlId + "_values"
    val oldList = form(oldListName)
    val oldIndexes = oldList.indexes
    val oldRaw =
      for (i <- oldIndexes;
           v <- oldList("[" + i + "]").value)
        yield v
    FormFieldInfo(prop, Some(QList.from(oldRaw, TagSetType)), false, true)
  }
}