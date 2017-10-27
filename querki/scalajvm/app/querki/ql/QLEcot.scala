package querki.ql

// These are both needed for embedding context for _QLButton. They
// might get pulled out into their own Ecot at some point:
import com.github.marklister.base64.Base64
import boopickle.Default._

import querki.globals._

import querki.ecology._

import models._

import querki.basic.PlainText
import querki.core.{IsTextType, QLText}
import querki.html.QHtml
import querki.tools.ProfileHandle
import querki.util.{QLog, UnexpectedPublicException}
import querki.values.{CutProcessing, ElemValue, EmptyContext, IsErrorType, QLContext, QValue, SpaceState}

object MOIDs extends EcotIds(24) {
  val SelfMethodOID = sysId(75)
  val CodeMethodOID = sysId(77)
  
  val IsBoundFunctionOID = moid(1)
}

private [ql] trait QLInternals extends EcologyInterface {
  def qlProfilers:QLProfilers
}

trait QLTestTools extends EcologyInterface {
  def serializeContextCore(qv:QValue, bindings:Map[String, QValue])(implicit state:SpaceState):String
}

class QLEcot(e:Ecology) extends QuerkiEcot(e) with QL with QLInternals with QLTestTools with querki.core.WithQL
  with querki.core.CollectionBase
  with querki.core.MethodDefs with querki.core.TextTypeBasis with querki.core.NameUtils with querki.core.NameTypeBasis 
  with querki.basic.PlainTextBaseType
{
  import MOIDs._
  
  lazy val HtmlUI = interface[querki.html.HtmlUI]
  lazy val Profiler = interface[querki.tools.Profiler]
  
  lazy val parserCreateProfiler = Profiler.createHandle("QLEcot.parserCreate")
  lazy val parserProcessProfiler = Profiler.createHandle("QLEcot.parserProcess")
  lazy val parserProcessMethodProfiler = Profiler.createHandle("QLEcot.parserProcessMethod")
  
  // This is provided as a service for the QLParsers:
  lazy val qlProfilers = new QLProfilers
  
  def QL = this
  
  /***********************************************
   * PUBLIC API
   ***********************************************/
  
  def qvUnpack(data:IVData[QValue]):QValue = {
    val qvs = data.vs
    val returnType = data.metadata.returnType
    val preferredColl = data.metadata.preferredColl
    // This code originally lived in QLContext.collect(). It is still kind of iffy, but is
    // conceptually Iterable[QValue].flatten:
    val pt = {
      if (qvs.isEmpty)
        returnType match {
          // This means that the calling code called Invocation.returnsType:
          case Some(ipt) => ipt
          // TODO: we might want to turn this into a logged warning. It can cause problems downstream if,
          // eg, you feed the results into _sort:
          case None => Core.UnknownType
        }
      else
        qvs.find(qv => qv.pType != Core.UnknownType).map(_.pType).getOrElse(returnType.getOrElse(Core.UnknownType))
    }
    val ct = {
      if (preferredColl.isDefined)
        preferredColl.get
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
  
  def inv2QValueImpl(invRaw:InvocationValue[QValue]):QFut = {
    val inv = invRaw.asInstanceOf[InvocationValueImpl[QValue]]
    inv.fut.map(data => qvUnpack(data))
  }
  
  def process(input:QLText, ci:QLContext, invOpt:Option[Invocation] = None, 
      lexicalThing:Option[PropertyBundle] = None, lexicalProp:Option[AnyProp] = None):Future[Wikitext] = 
  {
    val parser = parserCreateProfiler.profile { new QLParser(input, ci, invOpt, lexicalThing, lexicalProp) }
    parserProcessProfiler.profile { parser.process }
  }
  
  def processMethod(input:QLText, ci:QLContext, invOpt:Option[Invocation] = None, 
      lexicalThing:Option[PropertyBundle] = None, lexicalProp:Option[AnyProp] = None):Future[QValue] = 
  {
    val parser = parserCreateProfiler.profile { new QLParser(input, ci, invOpt, lexicalThing, lexicalProp) }
    parserProcessMethodProfiler.profile { parser.processMethod.map(_.value) }
  }
  
  def processMethodToWikitext(input:QLText, ci:QLContext, invOpt:Option[Invocation] = None, 
      lexicalThing:Option[PropertyBundle] = None, lexicalProp:Option[AnyProp] = None,
      initialBindings:Option[Map[String, QValue]] = None):Future[Wikitext] = 
  {
    val parser = parserCreateProfiler.profile { new QLParser(input, ci, invOpt, lexicalThing, lexicalProp, initialBindings) }
    parserProcessMethodProfiler.profile { parser.processMethodToWikitext }
  }
  
  def parseMethod(input:String):Option[QLPhrase] =
  {
    // Since we're just parsing, we shouldn't need a context:
    val parser = parserCreateProfiler.profile { new QLParser(QLText(input), EmptyContext(ecology)) }
    parser.parsePhrase()
  }
  
  lazy val ExactlyOneCut = new ExactlyOneBase(UnknownOID) {
    override def makePropValue(cv:Iterable[ElemValue], elemT:PType[_]):QValue = new ExactlyOnePropValue(cv.toList, this, elemT) with CutProcessing
  }
  
  lazy val EmptyListCutColl = new QListBase(UnknownOID, emptyProps) {
    def apply() = new QListPropValue(List.empty, this, Core.UnknownType) with CutProcessing  
  }
  def EmptyListCut() = EmptyListCutColl()

  object ErrorTextType extends PlainTextType(UnknownOID,
    toProps(
      setName("Error Text")
    )) with PTypeBuilder[PlainText,String] with IsErrorType {
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
  
  val QLTag = "QL -- Parsing and Running Expressions"
  
  /***********************************************
   * Serialization / Deserialization
   * 
   * These functions are used by systems such as _QLButton(), to embed the current state
   * of processing into HTML. They render an Invocation into an opaque String, and back again.
   */
  
  // findBindingsIn() is a collection of functions to traverse a QLExp and find all of the
  // bindings referred to in it, so we know what we need to serialize. Conceptually it's a
  // typeclass, but since we only need it here, I'm not bothering with the TC boilerplate.
  private def findBindingsInSeq[T](ts:Seq[T])(getter:T => Set[String]):Set[String] = {
    (Set.empty[String] /: ts) { (set, t) =>
      set ++ getter(t)
    }
  }
  
  private def findBindingsIn(exp:QLExp):Set[String] = {
    findBindingsInSeq(exp.phrases)(findBindingsIn(_))
  }
  private def findBindingsIn(phrase:QLPhrase):Set[String] = {
    findBindingsInSeq(phrase.ops)(findBindingsIn(_))
  }
  private def findBindingsIn(stage:QLStage):Set[String] = {
    stage match {
      case QLCall(name, methodNameOpt, paramsOpt, _) => {
        findBindingsIn(name) ++ 
          methodNameOpt.map(findBindingsIn(_)).getOrElse(Set.empty) ++
          paramsOpt.map { params =>
            findBindingsInSeq(params)(param => findBindingsIn(param.exp))
          }.getOrElse(Set.empty)
      }
      case QLTextStage(contents, _) => findBindingsIn(contents)
      case QLExpStage(exp) => findBindingsIn(exp)
      case QLListLiteral(exps) => findBindingsInSeq(exps)(findBindingsIn(_))
      case QLNumber(_) => Set.empty
    }
  }
  private def findBindingsIn(contents:ParsedQLText):Set[String] = {
    findBindingsInSeq(contents.parts) { part =>
      part match {
        case UnQLText(_) => Set.empty
        case QLLink(contents) => findBindingsIn(contents)
        case QLExp(exp) => findBindingsIn(QLExp(exp))
      }          
    }    
  }
  private def findBindingsIn(name:QLName):Set[String] = {
    // This is where we actually *find* the bindings
    name match {
      case QLSafeName(_) | QLDisplayName(_) | QLBindingDef(_, _, _) | QLThingId(_) => Set.empty
      case QLBinding(binding) => Set(binding)
    }
  }
  
  case class EmbeddedBinding(pTypeId:OID, cTypeId:OID, serialized:String)
  case class EmbeddableContext(qvpType:OID, qvcType:OID, serializedQV:String, bindingsSerialized:Map[String, EmbeddedBinding])
  
  // These guts are broken out for testing:
  def serializeContextCore(qv:QValue, bindings:Map[String, QValue])(implicit state:SpaceState):String = {
    val serializedQV = qv.serialize(qv.pType)
    // We need to store each binding with its PType and serialized Value:
    val bindingsSerialized:Map[String, EmbeddedBinding] = bindings.map { case (k, v) =>
      (k -> EmbeddedBinding(v.pType.id, v.cType.id, v.serialize(v.pType)))
    }
      
    // Now build the structure we're actually going to serialize...
    val embeddable = EmbeddableContext(qv.pType.id, qv.cType.id, serializedQV, bindingsSerialized)
    // ... pickle that...
    val buf = Pickle.intoBytes(embeddable)
    val arr = Array.ofDim[Byte](buf.remaining())
    buf.get(arr)
    // and turn it into an opaque embeddable String:
    Base64.Encoder(arr).toBase64    
  }
  
  def serializeContext(inv:Invocation, qlParamNameOpt:Option[String]):InvocationValue[String] = {
    try {
      implicit val state = inv.state
      // Note: we are explicitly assuming that there is only one implementation of Invocation! 
      val invImpl = inv.asInstanceOf[InvocationImpl]
      
      // First, serialize the received context. That's the easy bit:
      val context = inv.context
      val qv = context.value
  
      // Next, figure out which bindings are actually being *used* by the QL expression we're
      // embedding:
      def findBindingValue(inv:Invocation, name:String):Option[QValue] = {
        for {
          parser <- inv.context.parser
          scopes = inv.context.scopes(parser)
          bound <- scopes.lookup(name)
        }
          yield bound
      }
      def bindingValues(inv:Invocation, boundNames:Set[String]):Map[String, QValue] = {
        (Map.empty[String, QValue] /: boundNames) { (m, name) =>
          findBindingValue(inv, name).map(v => m + (name -> v)).getOrElse(m)
        }
      }
      
      val bindingsOpt = for {
        qlParamName <- qlParamNameOpt
        exp <- invImpl.sig.getParam(qlParamName).exp
        boundNames = findBindingsIn(exp)
      }
        yield bindingValues(inv, boundNames)
      val bindings = bindingsOpt.getOrElse(Map.empty)
      
      inv.wrap(serializeContextCore(qv, bindings))
    } catch {
      case SerializationException(tpe) => {
        inv.error("DataModel.serializationError", tpe.displayName)
      }
    }
  }
  
  def deserializeContext(str:String)(implicit state:SpaceState):(QValue, Map[String, QValue]) = {
    val byteArray = Base64.Decoder(str).toByteArray
    val embedded = Unpickle[EmbeddableContext].fromBytes(java.nio.ByteBuffer.wrap(byteArray))
    val pType = state.typ(embedded.qvpType)
    val cType = state.coll(embedded.qvcType)
    val qv = cType.deserialize(embedded.serializedQV, pType)
    val bindings = embedded.bindingsSerialized.map { case (name, EmbeddedBinding(ptid, ctid, serialized)) =>
      val pt = state.typ(ptid)
      val ct = state.coll(ctid)
      (name -> ct.deserialize(serialized, pt))
    }
    (qv, bindings)
  }
  
  /***********************************************
   * TYPES
   ***********************************************/

  /**
   * This is a fake PType, used when we encounter a name we don't know.
   */
  lazy val UnknownNameType = new NameTypeBase(UnknownOID, 
    toProps(
      setName("_unknownNameType"),
      Categories(QLTag),
      Summary("This is an error type produced when your expression contains an unknown name"))) 
  {
    def doWikify(context:QLContext)(v:String, displayOpt:Option[Wikitext] = None, lexicalThing:Option[PropertyBundle] = None) = {
      Future.successful(Wikitext("{{_unknownName:") + nameToLink(context)(v, displayOpt) + Wikitext("}}"))
    }
  }

  /**
   * This is a fake PType, which exists so that we can persist embedded Texts in the pipeline.
   */
  lazy val ParsedTextType = new SystemType[Wikitext](UnknownOID, 
    toProps(
      setName("Parsed Text Type"),
      Categories(QLTag),
      Summary("This is an internal Text Type that results from the system parsing some Text"))) with SimplePTypeBuilder[Wikitext]
  {
    def doDeserialize(v:String)(implicit state:SpaceState) = throw new SerializationException(this)
    def doSerialize(v:Wikitext)(implicit state:SpaceState) = throw new SerializationException(this)
    def doWikify(context:QLContext)(v:Wikitext, displayOpt:Option[Wikitext] = None, lexicalThing:Option[PropertyBundle] = None) = 
      Future.successful(v)
    override def doToUrlParam(v:Wikitext, raw:Boolean)(implicit state:SpaceState):String = {
      if (raw)
        s"${v.plaintext}"
      else
        s"""""${v.plaintext}"""""
    }
  
    override def doComp(context:QLContext)(left:Wikitext, right:Wikitext):Boolean = { left.plaintext.toLowerCase < right.plaintext.toLowerCase }
    override def doMatches(left:Wikitext, right:Wikitext):Boolean = { left.plaintext == right.plaintext }
    override def doDebugRender(context:QLContext)(v:Wikitext) = v.contents.map(_.internal).mkString
  
    def doDefault(implicit state:SpaceState) = Wikitext("")
    def wrap(raw:String):valType = Wikitext(raw)
    // This is a transient PType, so we don't care:
    def doComputeMemSize(v:Wikitext):Int = 0
    
    // Parsed Text can be coerced to conventional Text types:
    override def canCoerceTo(other:PType[_]):Boolean = {
      other match {
        case et:IsErrorType => false
        case itt:IsTextType => true
        case _ => false
      }
    }
    
    override def coerceTo(other:PType[_], elem:ElemValue):ElemValue = {
      other match {
        case et:IsErrorType => throw new Exception(s"PType $displayName can not be coerced to ${other.displayName}!")
        case itt:IsTextType => {
          val wiki = get(elem)
          itt(wiki.strip)
        }
        case _ => throw new Exception(s"PType $displayName can not be coerced to ${other.displayName}!")
      }
    }
  }
  
  lazy val ClosureType = new SystemType[QLClosure](UnknownOID,
    toProps(
      setName("_QL Closure"),
      setInternal)) with SimplePTypeBuilder[QLClosure]
  {
    def doDeserialize(v:String)(implicit state:SpaceState) = ???
    def doSerialize(v:QLClosure)(implicit state:SpaceState) = ???
    def doWikify(context:QLContext)(v:QLClosure, displayOpt:Option[Wikitext] = None, lexicalThing:Option[PropertyBundle] = None) = ???
    def doDefault(implicit state:SpaceState) = ???
    // This is a transient PType, so we don't care:
    def doComputeMemSize(v:QLClosure):Int = 0
  }
  
  /***********************************************
   * FUNCTIONS
   ***********************************************/
  
  lazy val SelfMethod = new InternalMethod(SelfMethodOID,
    toProps(
      setName("_self"),
      Categories(QLTag),
      Summary("Get a Link to this Thing"),
      Details("""*thing*._self simply produces *thing*.
          |
          |This seems silly, but it is useful for overriding the usual \_apply behavior. In particular,
          |*property*.\_self is the way to get a link to the property itself, instead of fetching the value
          |of the property on the received Thing. **This is the only common use case for _self!**
          |
          |More formally, \_self is the way to override the usual \[[\_apply\]] behaviour on a Thing, to get a
          |Link to that Thing. It is never necessary for ordinary Things, but frequently useful when \_apply
          |has been defined on it.
          |
          |_self can only be used in a dotted expression -- there must always be something before the dot, and
          |that something is what gets produced.
          |
          |If you think you want \_self for anything other than on a Property, odds are you are looking for
          |something else. If you're trying to fetch "everything that points to this" (one of the more common
          |cases), check out [[_refs._self]], which does exactly this. Or, if you are in a QL expression and are
          |looking for a way to say "this Thing that I'm looking at here", you can usually get that as
          |`$_context`. (Which is technically "the value passed into this QL expression at the beginning".)""".stripMargin)))
  {
    override def qlApply(inv:Invocation):QFut = {
      inv.definingContext match {
        case Some(context) => Future.successful(context.value)
        case _ => Future.successful(WarningValue("QL.self.notDotted")) 
      }
    }
  }
  
  lazy val IsBoundFunction = new InternalMethod(IsBoundFunctionOID,
    toProps(
      setName("_isBound"),
      Categories(QLTag),
      Summary("Is the specified name currently bound?"),
      Signature(
        expected = None,
        reqs = Seq(
          ("name", AnyType, "A name that might be bound, like $query.")
        ),
        opts = Seq.empty,
        returns = (YesNoType, "True iff the given name is currently bound, false otherwise.")
      )))
  {
    override def qlApply(inv:Invocation):QFut = {
      lazy val errOpt = Some(new PublicException("QL.isBound.notABinding"))
      for {
        param <- inv.rawRequiredParam("name")
        phrase <- inv.opt(param.phrases.headOption, errOpt)
        stage <- inv.opt(phrase.ops.headOption, errOpt)
        QLCall(binding, _, _, _) = stage
        QLBinding(name) = binding
        parser <- inv.opt(inv.context.parser)
        scopes <- inv.opt(inv.context.scopes.get(parser))
        answer = {
          // Look in the lexical scope first...
          scopes.lookup(name).orElse {
            // ... not there. Is there a request, and if so, is it defined as a param there?
            inv.context.requestOpt.flatMap(_.parsedParams.get(name))
          }.isDefined
        }
      }
        yield ExactlyOne(YesNoType(answer))
    }
  }
  
  lazy val CodeMethod = new InternalMethod(CodeMethodOID,
    toProps(
      setName("_code"),
      Categories(QLTag),
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
    def encodeString(str:String):QFut = {
      val escaped = scala.xml.Utility.escape(str)
      Future.successful(HtmlUI.HtmlValue(QHtml("<pre>" + escaped + "</pre>")))    
    }
    
    def encode(propVal:QValue, pType:PType[_]):QFut = {
      if (propVal.isEmpty)
        WarningFut("_code got an empty input")
      else {
        pType match {
          case codeType:CodeType => {
            val str = codeType.code(propVal.first)
            encodeString(str)
          }
          case ParsedTextType => {
            encodeString(propVal.firstTyped(ParsedTextType).map(_.plaintext).getOrElse(""))
          }
          case _ => WarningFut("_code doesn't work with type " + pType.displayName)
        }
      }
    }
    
    def encodeThingAndProp(thing:Thing, prop:Thing)(implicit space:SpaceState):Option[QFut] = {
      val propAndValOpt = thing.getPropOpt(prop.id)
      propAndValOpt.map { propAndVal => 
        encode(propAndVal.v, propAndVal.prop.pType)
      }
    }
    
    // TODO: this is horrible. Surely we can turn this into something cleaner with better use of the functional
    // tools in the Scala toolbelt.
    override def qlApply(inv:Invocation):QFut = {
      val partialContext = inv.preferDefiningContext.context
      implicit val space = partialContext.state
      inv.paramsOpt match {
        case Some(params) => {
          // TODO: the way we're handling this is horrible and hard-coded, and needs re-examination. The thing is,
          // we *mostly* don't want to process this. Specifically, we don't want to process the last step of this.
          // For the moment, we're hard-codedly checking the first stage of the phrase and using that, but it should
          // probably process everything until the last stage, and return that stage.
          val param = params.head
          val stage = param.exp.phrases.head.ops.head
          stage match {
            case QLTextStage(contents, _) => encodeString(contents.reconstructString)
            case QLNumber(num) => encodeString(stage.reconstructString)
            case lit:QLListLiteral => encodeString(lit.reconstructString) 
            case QLCall(name, methodNameOpt, _, _) => {
              val thingName = name.name
              methodNameOpt match {
                case Some(methodName) => {
                  val resultOpt = for (
                    thing <- space.anythingByName(thingName);
                    propThing <- space.anythingByName(methodName.name);
                    encoded <- encodeThingAndProp(thing, propThing)
                  )
                    yield encoded
                    
                  resultOpt.getOrElse(encodeString(param.reconstructString))
                }
                case None => {
                  val propOpt = space.anythingByName(thingName)
                  propOpt match {
                    case Some(propThing) => {
                      for {
                        thing <- inv.contextAllThings
                        res <- inv.fut(encodeThingAndProp(thing, propThing).getOrElse(encodeString(param.reconstructString)))
                      }
                        yield res
                    }
                    case None => encodeString(param.reconstructString)
                  }
                }
              }
            }
            case exp:QLExpStage => encodeString(exp.reconstructString)
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
    CodeMethod,
    IsBoundFunction
  )
}
