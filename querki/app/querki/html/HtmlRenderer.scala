package querki.html

import scala.xml._

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
  
  /*********************************
   * INTERNALS
   *********************************/
  
  def addEditorAttributes(elem:Elem, name:String, prop:String, id:String, thingOpt:Option[String]):Elem = {
    val xml2 = elem %
    	Attribute("name", Text(name),
    	Attribute("data-prop", Text(prop),
    	Attribute("class", Text("propEditor"),
    	Attribute("id", Text(id), Null))))
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
    
    <div>
      <div class="buttonset">
        {oneButton("Yes", "true", (isSet && v))}
        {oneButton("Maybe", "maybe", (!isSet))}
        {oneButton("No", "false", (isSet && !v))}      
      </div>
    </div>
  }
  
  def handleSpecialized(prop:Property[_,_], newVal:String):Option[PropValue] = {
    if (prop.cType == Optional && prop.pType == YesNoType)
      Some(handleYesNo(prop, newVal))
    else
      None
  }
  
  def handleYesNo(prop:Property[_,_], newVal:String):PropValue = {
    newVal match {
      case "maybe" => Optional.default(prop.pType)
      case _ => Optional(YesNoType.fromUser(newVal))
    }
  }
}