package querki.ql

// TODO: this is an abstraction break!!! It should be replaced by use of Scala's XHtml instead.
import play.twirl.api.Html

import querki.globals._

import querki.ecology._

import models.{PropertyBundle, PType, PTypeBuilder, SimplePTypeBuilder, Thing, UnknownOID, Wikitext}

import querki.core.QLText
import querki.tools.ProfileHandle
import querki.util.QLog
import querki.values.{CutProcessing, ElemValue, IsErrorType, QLContext, SpaceState}

object MOIDs extends EcotIds(24) {
  val SelfMethodOID = sysId(75)
  val CodeMethodOID = sysId(77)
}

private [ql] trait QLInternals extends EcologyInterface {
  def qlProfilers:QLProfilers
}

class QLEcot(e:Ecology) extends QuerkiEcot(e) with QL with QLInternals
  with querki.core.CollectionBase
  with querki.core.MethodDefs with querki.core.TextTypeBasis with querki.core.NameUtils with querki.core.NameTypeBasis 
{
  import MOIDs._
  
  lazy val HtmlUI = interface[querki.html.HtmlUI]
  lazy val Profiler = interface[querki.tools.Profiler]
  
  lazy val parserCreateProfiler = Profiler.createHandle("QLEcot.parserCreate")
  lazy val parserProcessProfiler = Profiler.createHandle("QLEcot.parserProcess")
  lazy val parserProcessMethodProfiler = Profiler.createHandle("QLEcot.parserProcessMethod")
  
  // This is provided as a service for the QLParsers:
  lazy val qlProfilers = new QLProfilers
  
  /***********************************************
   * PUBLIC API
   ***********************************************/
  
  def inv2QValueImpl(inv:InvocationValue[QValue]):QValue = {
    inv.getError.getOrElse {
      val qvs = inv.get
      // This code originally lived in QLContext.collect(). It is still kind of iffy, but is
      // conceptually Iterable[QValue].flatten:
      val pt = {
        if (qvs.isEmpty)
          inv.getReturnType match {
            // This means that the calling code called Invocation.returnsType:
            case Some(ipt) => ipt
            // TODO: we might want to turn this into a logged warning. It can cause problems downstream if,
            // eg, you feed the results into _sort:
            case None => Core.UnknownType
          }
        else
          qvs.find(qv => qv.pType != Core.UnknownType).map(_.pType).getOrElse(inv.getReturnType.getOrElse(Core.UnknownType))
      }
      val ct = {
        if (inv.preferredColl.isDefined)
          inv.preferredColl.get
        else if (qvs.isEmpty)
          Core.Optional
        else
          qvs.head.cType
      }
      val raw = qvs.flatten(_.cv)
      // Rationalize the Collection. If the Collection we got from the invocation works with the number of
      // elements, keep it; otherwise, slam it to something sensible.
      // TODO: this mechanism needs to be generalized.
      val newCT = 
        if (raw.isEmpty) {
          if (ct == Core.ExactlyOne)
            Core.Optional
          else
            ct
        } else if (raw.size == 1)
          // Any Collection can have one element:
          ct
        else {
          // There are two or more elements:
          if (ct == Core.ExactlyOne || ct == Core.Optional)
            Core.QList
          else
            ct
        }
      newCT.makePropValue(raw, pt)
    }
  }
  
  def process(input:QLText, ci:QLContext, invOpt:Option[Invocation] = None, 
      lexicalThing:Option[PropertyBundle] = None, lexicalProp:Option[AnyProp] = None):Wikitext = 
  {
    val parser = parserCreateProfiler.profile { new QLParser(input, ci, invOpt, lexicalThing, lexicalProp) }
    parserProcessProfiler.profile { parser.process }
  }
  
  def processMethod(input:QLText, ci:QLContext, invOpt:Option[Invocation] = None, 
      lexicalThing:Option[PropertyBundle] = None, lexicalProp:Option[AnyProp] = None):QValue = 
  {
    val parser = parserCreateProfiler.profile { new QLParser(input, ci, invOpt, lexicalThing, lexicalProp) }
    parserProcessMethodProfiler.profile { parser.processMethod.value }
  }
  
  lazy val ExactlyOneCut = new ExactlyOneBase(UnknownOID) {
    override def makePropValue(cv:Iterable[ElemValue], elemT:PType[_]):QValue = new ExactlyOnePropValue(cv.toList, this, elemT) with CutProcessing
  }
  
  lazy val EmptyListCutColl = new QListBase(UnknownOID, () => Thing.emptyProps) {
    def apply() = new QListPropValue(List.empty, this, Core.UnknownType) with CutProcessing  
  }
  def EmptyListCut() = EmptyListCutColl()

  object ErrorTextType extends TextTypeBase(UnknownOID,
    toProps(
      setName("Error Text")
    )) with PTypeBuilder[QLText,String] with IsErrorType {
  }

  def WarningValue(msg:String) = ExactlyOneCut(ErrorTextType("{{_warning:" + msg + "}}"))
  
  def ErrorValue(msg:String) = {
    try {
      throw new Exception("dummy")
    } catch {
      case e:Exception => QLog.error(s"Displaying error $msg; stack trace:\n${e.getStackTrace.toString()}")  
    }
    WarningValue(msg)
  }
  
  def WikitextValue(wikitext:Wikitext):QValue = ExactlyOne(ParsedTextType(wikitext))
  
  /***********************************************
   * TYPES
   ***********************************************/

  /**
   * This is a fake PType, used when we encounter a name we don't know.
   */
  lazy val UnknownNameType = new NameTypeBase(UnknownOID, toProps(setName("_unknownNameType"))) {
    def doWikify(context:QLContext)(v:String, displayOpt:Option[Wikitext] = None, lexicalThing:Option[PropertyBundle] = None) = {
      Wikitext("{{_unknownName:") + nameToLink(context)(v, displayOpt) + Wikitext("}}")
    }
  }

  /**
   * This is a fake PType, which exists so that we can persist embedded Texts in the pipeline.
   */
  lazy val ParsedTextType = new SystemType[Wikitext](UnknownOID, toProps(setName("Parsed Text Type"))) with SimplePTypeBuilder[Wikitext]
  {
    def doDeserialize(v:String)(implicit state:SpaceState) = throw new Exception("Can't deserialize ParsedText!")
    def doSerialize(v:Wikitext)(implicit state:SpaceState) = throw new Exception("Can't serialize ParsedText!")
    def doWikify(context:QLContext)(v:Wikitext, displayOpt:Option[Wikitext] = None, lexicalThing:Option[PropertyBundle] = None) = v
  
    override def doComp(context:QLContext)(left:Wikitext, right:Wikitext):Boolean = { left.plaintext < right.plaintext }
    override def doMatches(left:Wikitext, right:Wikitext):Boolean = { left.plaintext == right.plaintext }
    override def doDebugRender(context:QLContext)(v:Wikitext) = v.contents.map(_.internal).mkString
  
    def doDefault(implicit state:SpaceState) = Wikitext("")
    def wrap(raw:String):valType = Wikitext(raw)
  }
  
  /***********************************************
   * FUNCTIONS
   ***********************************************/
	
	lazy val SelfMethod = new InternalMethod(SelfMethodOID,
	    toProps(
	      setName("_self"),
	      Summary("Get a Link to this Thing"),
	      Details("""*thing*._self simply produces *thing*.
	          |
	          |This seems silly, but it is useful for overriding the usual \_apply behavior. In particular,
	          |*property*.\_self is the way to get a link to the property itself, instead of fetching the value
	          |of the property on the received Thing.
	          |
	          |More formally, \_self is the way to override the usual \[[\_apply\]] behaviour on a Thing, to get a
	          |Link to that Thing. It is never necessary for ordinary Things, but frequently useful when \_apply
	          |has been defined on it.""".stripMargin)))
	{
	  override def qlApply(inv:Invocation):QValue = {
	    inv.definingContext.get.value
	  }
	}
	
	lazy val CodeMethod = new InternalMethod(CodeMethodOID,
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
	    HtmlUI.HtmlValue(Html("<pre>" + escaped + "</pre>"))    
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
	  override def qlApply(inv:Invocation):QValue = {
	    val partialContext = inv.preferDefiningContext.context
	    implicit val space = partialContext.state
	    inv.paramsOpt match {
	      case Some(params) => {
	        // TODO: the way we're handling this is horrible and hard-coded, and needs re-examination. The thing is,
	        // we *mostly* don't want to process this. Specifically, we don't want to process the last step of this.
	        // For the moment, we're hard-codedly checking the first stage of the phrase and using that, but it should
	        // probably process everything until the last stage, and return that stage.
	        val phrase = params.head
	        val stage = phrase.ops.head
	        stage match {
	          case QLTextStage(contents, _) => encodeString(contents.reconstructString)
	          case QLNumber(num) => encodeString(stage.reconstructString)
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
	                    for {
	                      thing <- inv.contextAllThings
	                    }
	                      yield encodeThingAndProp(thing, propThing).getOrElse(encodeString(phrase.reconstructString))
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