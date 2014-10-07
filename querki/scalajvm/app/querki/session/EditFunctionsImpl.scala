package querki.session

import models.{DisplayPropVal, DisplayText, FieldIds, FormFieldInfo, IndexedOID, Thing, ThingId, Wikitext}

import querki.globals._

import querki.api.{ChangePropertyValue, DeleteProperty, PropertyChange, PropertyChangeResponse, EditFunctions}
import querki.values.{QLRequestContext, RequestContext}

trait EditFunctionsImpl extends SessionApiImpl with EditFunctions {
  
  lazy val HtmlRenderer = interface[querki.html.HtmlRenderer]
  lazy val Types = interface[querki.types.Types]
  
  lazy val doLogEdits = Config.getBoolean("querki.test.logEdits", false)
  
  def alterProperty(thingId:String, path:String, change:PropertyChange):PropertyChangeResponse = withThing(thingId) { thing =>
    implicit val s = state
    change match {
      case ChangePropertyValue(vs) => {
  	    DisplayPropVal.propPathFromName(path, Some(thing)) match {
    	  case Some(fieldIds) => {
    	    val context = QLRequestContext(rc)
    	    val actualFormFieldInfo = HtmlRenderer.propValFromUser(fieldIds, vs, context)
    	    val result = fieldIds.container match {
    	      // If this value is contained inside (potentially nested) Bundles, dive down into them
    	      // and adjust the results:
              case Some(higherFieldIds) => {
                // TEMP: restructure rebuildBundle to take higherFieldIds directly:
                def toHigherIds(oneFieldId:FieldIds):List[IndexedOID] = {
                  val thisId = IndexedOID(oneFieldId.p.id, oneFieldId.index)
                  oneFieldId.container match {
                    case Some(c) => toHigherIds(c) ::: List(thisId)
                    case None => List(thisId)
                  }
                }
                Types.rebuildBundle(Some(thing), toHigherIds(higherFieldIds), actualFormFieldInfo).
                  getOrElse(FormFieldInfo(fieldIds.p, None, true, false, None, Some(new PublicException("Didn't get bundle"))))         
              }
              case None => actualFormFieldInfo
    	    }

    	    ???
    	  }
    	  case None => ???  // This is an error -- what should we return?
    	}
      }
      case DeleteProperty => ???  // NYI
    }   
  }
}
