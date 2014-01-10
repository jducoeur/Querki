package querki.ql

// TODO: this is an abstraction break!!! It should be replaced by use of Scala's XHtml instead.
import play.api.templates.Html

import querki.ecology._

import models.{PType, PTypeBuilder, Thing, UnknownOID, UnknownType}
import models.system.{QLText}

import ql._

import querki.util.QLog
import querki.values.{CutProcessing, ElemValue, HtmlValue, ParsedTextType, QLContext, SpaceState}

object MOIDs extends EcotIds(24) {
  val SelfMethodOID = sysId(75)
  val CodeMethodOID = sysId(77)
}

class QLEcot(e:Ecology) extends QuerkiEcot(e) with QL with querki.core.MethodDefs {
  import MOIDs._
  
  lazy val ExactlyOneCut = new querki.core.ExactlyOneBase(UnknownOID) {
    override def makePropValue(cv:Iterable[ElemValue], elemT:PType[_]):QValue = new ExactlyOnePropValue(cv.toList, this, elemT) with CutProcessing
  }
  
  lazy val EmptyListCutColl = new querki.core.QListBase(UnknownOID, () => Thing.emptyProps) {
    def apply() = new QListPropValue(List.empty, this, UnknownType) with CutProcessing  
  }
  def EmptyListCut() = EmptyListCutColl()

  object ErrorTextType extends models.system.TextTypeBase(UnknownOID,
    Thing.toProps(
      Thing.setName("Error Text")
    )) with PTypeBuilder[QLText,String] {
  }

  def WarningValue(msg:String) = ExactlyOneCut(ErrorTextType("{{_warning:" + msg + "}}"))
  
  def ErrorValue(msg:String) = {
    try {
      throw new Exception("dummy")
    } catch {
      case e:Exception => QLog.error(s"Displaying error $msg; stack trace:\n${e.getStackTraceString}")  
    }
    WarningValue(msg)
  }
  
  /***********************************************
   * FUNCTIONS
   ***********************************************/
	
	lazy val SelfMethod = new SingleContextMethod(SelfMethodOID,
	    toProps(
	      setName("_self"),
	      Summary("Get a Link to this Thing"),
	      Details("""*thing*._self simply produces *thing*.
	          |
	          |This seems silly, but it is useful for overriding the usual _apply behavior. In particular,
	          |*property*._self is the way to get a link to the property itself, instead of fetching the value
	          |of the property on the received Thing.
	          |
	          |More formally, _self is the way to override the usual [[_apply]] behaviour on a Thing, to get a
	          |Link to that Thing. It is never necessary for ordinary Things, but frequently useful when _apply
	          |has been defined on it.""".stripMargin)))
	{
	  def fullyApply(mainContext:QLContext, partialContext:QLContext, params:Option[Seq[QLPhrase]]):QValue = {
	    partialContext.value
	  }
	}
	
	lazy val CodeMethod = new SingleContextMethod(CodeMethodOID,
	    toProps(
	      setName("_code"),
	      Summary("Display a block of QL code"),
	      Details("""_code() displays the raw code of a value or property, pretty flexibly.
	          |
	          |You can give it as "TEXT -> _code" to display the TEXT -- however, note that the TEXT will be processed as normal
	          |in this case. If you want to show some raw code, unprocessed, do it as "_code(TEXT)" instead.
	          |
	          |You can give a property as a parameter -- "_code(PROP)" -- and it will display the value of the property on this Thing.
	          |
	          |Or you can give a property on some other Thing -- "_code(THING.PROP)" -- to display the value of the property on that Thing.
	          |
	          |If you have a parameter, and it doesn't work as either PROP or THING.PROP, then it will display the parameter literally.
	          |
	          |The results are displayed in an inset block, in monospaced type, so that it looks "codish".
	          |
	          |_code is, frankly, a bit persnickety at this point, and not always easy to use for complicated examples. It should be
	          |considered a work in progress.""".stripMargin)))
	{
	  def encodeString(str:String):QValue = {
	    val escaped = scala.xml.Utility.escape(str)
	    HtmlValue(Html("<pre>" + escaped + "</pre>"))    
	  }
	  
	  def encode(propVal:QValue, pType:PType[_]):QValue = {
	    if (propVal.isEmpty)
	      WarningValue("_code got an empty input")
	    else {
	      pType match {
	        case codeType:CodeType => {
	          val str = codeType.code(propVal.first)
	          encodeString(str)
	        }
	        case ParsedTextType => {
	          encodeString(propVal.firstTyped(ParsedTextType).map(_.plaintext).getOrElse(""))
	        }
	        case _ => WarningValue("_code doesn't work with type " + pType.displayName)
	      }
	    }
	  }
	  
	  def encodeThingAndProp(thing:Thing, prop:Thing)(implicit space:SpaceState):Option[QValue] = {
	    val propAndValOpt = thing.getPropOpt(prop.id)
	    propAndValOpt.map { propAndVal => 
	      encode(propAndVal.v, propAndVal.prop.pType)
	    }
	  }
	  
	  // TODO: this is horrible. Surely we can turn this into something cleaner with better use of the functional
	  // tools in the Scala toolbelt.
	  def fullyApply(mainContext:QLContext, partialContext:QLContext, paramsOpt:Option[Seq[QLPhrase]]):QValue = {
	    implicit val space = partialContext.state
	    paramsOpt match {
	      case Some(params) => {
	        // TODO: the way we're handling this is horrible and hard-coded, and needs re-examination. The thing is,
	        // we *mostly* don't want to process this. Specifically, we don't want to process the last step of this.
	        // For the moment, we're hard-codedly checking the first stage of the phrase and using that, but it should
	        // probably process everything until the last stage, and return that stage.
	        val phrase = params.head
	        val stage = phrase.ops.head
	        stage match {
	          case QLTextStage(contents, _) => encodeString(contents.reconstructString)
	          case QLBinding(_) => WarningValue("It is meaningless to call _code on a Binding.")
	          case QLCall(name, methodNameOpt, _, _) => {
	            val thingName = name.name
	            methodNameOpt match {
	              case Some(methodName) => {
	                val resultOpt = for (
	                  thing <- space.anythingByName(thingName);
	                  propThing <- space.anythingByName(methodName);
	                  encoded <- encodeThingAndProp(thing, propThing)
	                )
	                  yield encoded
	                  
	                resultOpt.getOrElse(encodeString(phrase.reconstructString))
	              }
	              case None => {
	                val propOpt = space.anythingByName(thingName)
	                propOpt match {
	                  case Some(propThing) => {
	                    applyToIncomingThing(mainContext) { (thing, _) =>
	                      encodeThingAndProp(thing, propThing).getOrElse(encodeString(phrase.reconstructString))
	                    }
	                  }
	                  case None => encodeString(phrase.reconstructString)
	                }
	              }
	            }
	          }
	        }
	      }
	      case None => {
	        encode(partialContext.value, partialContext.value.pType)
	      }
	    }
	  }
	}

  override lazy val props = Seq(
    SelfMethod,
    CodeMethod
  )
}