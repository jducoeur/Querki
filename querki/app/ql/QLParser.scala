package ql

import language.existentials
import play.api.Logger
import play.api.templates.Html

import scala.util.parsing.combinator._

import models.system._
import models._
import controllers.RequestContext

/**
 * This is a fake PType, which exists so that we can persist embedded Texts in the pipeline.
 */
object ParsedTextType extends SystemType[Wikitext](OIDs.IllegalOID, () => Thing.emptyProps) with SimplePTypeBuilder[Wikitext]
{
  def doDeserialize(v:String) = throw new Exception("Can't deserialize ParsedText!")
  def doSerialize(v:Wikitext) = throw new Exception("Can't serialize ParsedText!")
  def doRender(context:ContextBase)(v:Wikitext) = v
    
  val doDefault = Wikitext("")
  def wrap(raw:String):valType = Wikitext(raw)
}

/**
 * This is a fake PType, so that code can inject HTML into the pipeline
 */
object RawHtmlType extends SystemType[Wikitext](OIDs.IllegalOID, () => Thing.emptyProps) with SimplePTypeBuilder[Wikitext]
{
  def doDeserialize(v:String) = throw new Exception("Can't deserialize ParsedText!")
  def doSerialize(v:Wikitext) = throw new Exception("Can't serialize ParsedText!")
  def doRender(context:ContextBase)(v:Wikitext) = v
    
  val doDefault = Wikitext("")
}

/**
 * This is a fake PType, used when we encounter a name we don't know.
 */
object UnknownNameType extends NameType(UnknownOID, "_unknownNameType") {
  def doRender(context:ContextBase)(v:String) = nameToLink(context)(v)
}

// TODO: we've gotten rid of the explicit ct parameter, since it is contained in v.
// Maybe we can do the same for pt?
case class TypedValue(v:PropValue, pt:PType[_], cut:Boolean = false) {
  def ct:Collection = v.coll
  
  def render(context:ContextBase):Wikitext = v.render(context, pt) 
  
  def firstTyped[VT](expectedType:PType[VT]):Option[VT] = {
    if (expectedType == pt) {
      v.firstTyped(expectedType)
    } else
      None
  }
}
object ErrorValue {
  def apply(msg:String) = {
    try {
      throw new Exception("dummy")
    } catch {
      case e:Exception => Logger.error(s"Displaying error $msg; stack trace:\n${e.getStackTraceString}")  
    }
    TypedValue(ExactlyOne(PlainTextType(msg)), PlainTextType, true)
  }
}
object TextValue {
  def apply(msg:String) = TypedValue(ExactlyOne(PlainTextType(msg)), PlainTextType)
}
object HtmlValue {
  def apply(html:Html) = TypedValue(ExactlyOne(RawHtmlType(HtmlWikitext(html))), RawHtmlType)
}
object WikitextValue {
  def apply(wikitext:Wikitext) = TypedValue(ExactlyOne(ParsedTextType(wikitext)), ParsedTextType)
}
object LinkValue {
  def apply(target:OID) = TypedValue(ExactlyOne(LinkType(target)), LinkType)
}
object WarningValue {
  def apply(msg:String) = TypedValue(ExactlyOne(TextType("{{_warning:" + msg + "}}")), TextType, true)
}
object EmptyValue {
  def apply(pType:PType[_]) = TypedValue(QList.empty, pType)
  // TODO: this is evil -- in the long run, we should eliminate this and think more carefully about what
  // the correct type is in cases where it is being used:
  def untyped = TypedValue(QList.empty, YesNoType)
}

abstract class ContextBase {
  def value:TypedValue
  def state:SpaceState
  def request:RequestContext
  // Parent matters at rendering time -- we render the final context in the context of its parent.
  // This matter most when (as often), the last context is a Text; it needs to be rendered correctly
  // in its parent context.
  def parent:ContextBase
  def parser:Option[QLParser]
  def useCollection:Boolean = false
  
  def depth:Int
  // This might become a config param -- it is the maximum depth we will allow a call to be. For now, we're
  // keeping it very tight, but it might eventually need to be over a thousand.
  val maxDepth = 100
  
  def isEmpty = value.v.isEmpty
  
  /**
   * Maps the given function to each element in this context.
   * 
   * IMPORTANT: if the useCollection flag is set for this context, then this calls the function only once,
   * with the collection context, *not* with each element! In other words, the definition of "map" depends
   * on the useCollection flag!
   */
  def map[T](cb:ContextBase => T) = {
    if (useCollection) {
      List(cb(this))
    } else {
      value.v.cv map { elem =>
        val elemContext = next(TypedValue(ExactlyOne(elem), value.pt))
        cb(elemContext)
      }
    }
  }
  
  /**
   * Maps the given function to each element in this context, and flattens the result.
   * 
   * TODO: this isn't quite right, monadically speaking -- it's shouldn't assume Option. At some
   * point when I can think it through better, make the signatures here more correct. But Option is
   * the case I usually care about.
   * 
   * IMPORTANT: if the useCollection flag is set for this context, then this calls the function only once,
   * with the collection context, *not* with each element! In other words, the definition of "map" depends
   * on the useCollection flag!
   */
  def flatMap[T](cb:ContextBase => Option[T]) = {
    if (useCollection) {
      val ret = cb(this)
      ret match {
        case Some(t) => List(t)
        case None => List.empty[T]
      }
    } else {
      value.v.cv flatMap { elem =>
        val elemContext = next(TypedValue(ExactlyOne(elem), value.pt))
        cb(elemContext)
      }
    }
  }
  
  /**
   * Similar to ordinary map(), but produces a new Context with the result.
   */
  def flatMapAsContext[T <: ElemValue](cb:ContextBase => Option[T], resultType:PType[_]):ContextBase = {
    val ct = value.ct
    // TODO: this is an unfortunate cast. It's correct, but ick. Can we eliminate it?
    val raw = flatMap(cb).asInstanceOf[ct.implType]
    val propVal = ct.makePropValue(raw)
    next(TypedValue(propVal, resultType))
  }
  
  override def toString = "Context(" + value.v + ")"
  
  /**
   * Convenience method to build the successor to this context, in typical chained situations.
   */
  def next(v:TypedValue) = QLContext(v, request, Some(this), parser, depth + 1)
  
  def asCollection = QLContext(value, request, Some(parent), parser, depth + 1, true)
  
  /**
   * Returns the root of the context tree. Mainly so that parameters can start again with the same root.
   */
  def root:ContextBase = 
    if (parent == this)
      this
    else
      parent.root
      
  def isCut = value.cut
}

/**
 * Represents the incoming "context" of a parsed QLText.
 */
case class QLContext(value:TypedValue, request:RequestContext, parentIn:Option[ContextBase] = None, parser:Option[QLParser] = None, depth:Int = 0, listIn:Boolean = false) extends ContextBase {
  def state = request.state.getOrElse(SystemSpace.State)
  def parent = parentIn match {
    case Some(p) => p
    case None => this
  }
  override def useCollection = listIn
}

case class QLRequestContext(request:RequestContext) extends ContextBase {
  def state = request.state.getOrElse(SystemSpace.State)
  def value:TypedValue = throw new Exception("Can't use the contents of QLRequestContext!")  
  def parent:ContextBase = throw new Exception("QLRequestContext doesn't have a parent!")
  def parser:Option[QLParser] = throw new Exception("QLRequestContext doesn't have a parser!")
  def depth:Int = 0
}

/**
 * This should only be used in cases where we really don't have a context -- generally,
 * displaying outside of a proper page display in a Space. It is unlikely to work for
 * any sophisticated properties, but can be used for PlainText and the like.
 */
case object EmptyContext extends ContextBase {
  def value:TypedValue = throw new Exception("Can't use the contents of EmptyContext!")
  def state:SpaceState = throw new Exception("Can't use the space of EmptyContext!")
  def request:RequestContext = throw new Exception("Can't get the request of EmptyContext!")
  def parent:ContextBase = throw new Exception("EmptyContext doesn't have a parent!")
  def parser:Option[QLParser] = throw new Exception("Can't get a parser from EmptyContext!")
  def depth:Int = 0
}

/**
 * A QLFunction is something that can be called in the QL pipeline. It is mostly implemented
 * by Thing (with variants for Property and suchlike), but can also be anonymous -- for example,
 * when a method returns a partially-applied function.
 */
trait QLFunction {
  def qlApply(context:ContextBase, params:Option[Seq[QLPhrase]] = None):TypedValue
}

class PartiallyAppliedFunction(partialContext:ContextBase, action:(ContextBase, Option[Seq[QLPhrase]]) => TypedValue) extends QLFunction {
  def qlApply(context:ContextBase, params:Option[Seq[QLPhrase]] = None):TypedValue = {
    action(context, params)
  }
}

class BogusFunction extends QLFunction {
  def qlApply(context:ContextBase, params:Option[Seq[QLPhrase]] = None):TypedValue = {
    ErrorValue("It does not make sense to put this after a dot.")
  }
}

sealed abstract class QLTextPart
case class UnQLText(text:String) extends QLTextPart
sealed abstract class QLStage(collFlag:Option[String]) {
  def useCollection:Boolean = collFlag match {
    case Some(_) => true
    case None => false
  }
}
case class QLCall(name:String, methodName:Option[String], params:Option[Seq[QLPhrase]], collFlag:Option[String]) extends QLStage(collFlag)
case class QLTextStage(contents:ParsedQLText, collFlag:Option[String]) extends QLStage(collFlag)
//case class QLPlainTextStage(text:String) extends QLStage
case class QLPhrase(ops:Seq[QLStage])
case class QLExp(phrases:Seq[QLPhrase]) extends QLTextPart
case class QLLink(contents:ParsedQLText) extends QLTextPart
case class ParsedQLText(parts:Seq[QLTextPart])

class QLParser(val input:QLText, ci:ContextBase) extends RegexParsers {
  
  // Add the parser to the context, so that methods can call back into it:
  val initialContext = QLContext(ci.value, ci.request, Some(ci.parent), Some(this), ci.depth + 1)
  
  // Crude but useful debugging of the process tree. Could stand to be beefed up when I have time
  def logContext(msg:String, context:ContextBase) = {
    //Logger.info(msg + ": " + context)
  }
  
  val name = """[a-zA-Z_][\w-_ ]*[\w]""".r
  val unQLTextRegex = """([^\[\"_]|\[(?!\[)|\"(?!\")|_(?!_))+""".r
  // We don't want the RegexParser removing whitespace on our behalf. Note that we need to be
  // very careful about whitespace!
  override val whiteSpace = "".r
  
  def unQLText:Parser[UnQLText] = unQLTextRegex ^^ { UnQLText(_) }
  def qlCall:Parser[QLCall] = opt("\\*\\s*".r) ~ name ~ opt("." ~> name) ~ opt("\\(\\s*".r ~> (rep1sep(qlPhrase, "\\s*,\\s*".r) <~ "\\s*\\)".r)) ^^ { 
    case collFlag ~ n ~ optMethod ~ optParams => QLCall(n, optMethod, optParams, collFlag) }
  def qlTextStage:Parser[QLTextStage] = (opt("\\*\\s*".r) <~ "\"\"") ~ qlText <~ "\"\"" ^^ {
    case collFlag ~ text => QLTextStage(text, collFlag) }
  // TODO: get this working properly, so we have properly plain text literals:
//  def qlPlainTextStage:Parser[QLPlainTextStage] = "\"" ~> unQLTextRegex <~ "\"" ^^ { QLPlainTextStage(_) }
  def qlStage:Parser[QLStage] = qlCall | qlTextStage // | qlPlainTextStage
  // TODO: phrase is going to get a *lot* more complex with time:
  def qlPhrase:Parser[QLPhrase] = rep1sep(qlStage, "\\s*->\\s*".r) ^^ { QLPhrase(_) }
  def qlExp:Parser[QLExp] = rep1sep(qlPhrase, "\\s*\\r?\\n|\\s*;\\s*".r) ^^ { QLExp(_) }
  def qlLink:Parser[QLLink] = qlText ^^ { QLLink(_) }
  def qlText:Parser[ParsedQLText] = rep(unQLText | "[[" ~> qlExp <~ "]]" | "__" ~> qlLink <~ "__") ^^ { ParsedQLText(_) }
  
  /**
   * Note that the output here is nominally a new Context, but the underlying type is
   * ParsedTextType. So far, you can't *do* anything with that afterwards other than
   * render it, which just returns the already-computed Wikitext.
   */
  private def processTextStage(text:QLTextStage, context:ContextBase):ContextBase = {
    logContext("processTextStage " + text, context)
    val ct = context.value.ct
    // For each element of the incoming context, recurse in and process the embedded Text
    // in that context.
    val transformed = context.map { elemContext =>
      ParsedTextType(processParseTree(text.contents, elemContext))
    }
    // TBD: the asInstanceOf here is surprising -- I would have expected transformed to come out
    // as the right type simply by type signature. Can we get rid of it?
    context.next(TypedValue(ct.makePropValue(transformed.asInstanceOf[ct.implType]), ParsedTextType))
  }
  
  private def processCall(call:QLCall, context:ContextBase):ContextBase = {
    logContext("processName " + call, context)
    val thing = context.state.anythingByName(call.name)
    val tv = thing match {
      case Some(t) => {
        // If there are parameters to the call, they are a collection of phrases.
        val params = call.params
        val methodOpt = call.methodName.flatMap(context.state.anythingByName(_))
        methodOpt match {
          case Some(method) => {
            val partialFunction = method.partiallyApply(context.next(TypedValue(ExactlyOne(LinkType(t.id)), LinkType)))
            partialFunction.qlApply(context, params)
          }
          case None => t.qlApply(context, params)
        }
      }
      // They specified a name we don't know. Turn it into a raw NameType and pass it through. If it gets to
      // the renderer, it will turn into a link to the undefined page, where they can create it.
      //
      // TBD: in principle, we might want to make this more Space-controllable. But it isn't obvious that we care. 
      case None => TypedValue(ExactlyOne(UnknownNameType(call.name)), UnknownNameType)
    }
    logContext("processName got " + tv, context)
    context.next(tv)
  }
  
  private def processStage(stage:QLStage, contextIn:ContextBase):ContextBase = {
    val context = 
      if (contextIn.depth > contextIn.maxDepth)
        contextIn.next(WarningValue("Too many levels of calls -- you can only have up to " + contextIn.maxDepth + " calls in a phrase."));
      else if (stage.useCollection) 
        contextIn.asCollection 
      else 
        contextIn
    logContext("processStage " + stage, context)
    stage match {
      case name:QLCall => processCall(name, context)
      case subText:QLTextStage => processTextStage(subText, context)
//      case QLPlainTextStage(text) => context.next(TextValue(text))
    }
  }
  
  def processPhrase(ops:Seq[QLStage], startContext:ContextBase):ContextBase = {
    logContext("processPhrase " + ops, startContext)
    (startContext /: ops) { (context, stage) => 
      if (context.isCut)
        // Setting the "cut" flag means that we're just stopping processing here. Usually
        // used for warnings:
        context
      else
        processStage(stage, context) 
    }
  }
  
  private def processPhrases(phrases:Seq[QLPhrase], context:ContextBase):Seq[ContextBase] = {
    logContext("processPhrases " + phrases, context)
    phrases map (phrase => processPhrase(phrase.ops, context))
  }

  def contextsToWikitext(contexts:Seq[ContextBase]):Wikitext = {
    (Wikitext("") /: contexts) { (soFar, context) => soFar + context.value.render(context.parent) }
  }
  
  /**
   * This deals with QText that contains "__stuff__" or "____", both of which render as
   * links to the incoming context. (Or simply the value of the context, if it's not a Link.)
   */
  private def linkToWikitext(contents:ParsedQLText, context:ContextBase):Wikitext = {
    contents.parts.length match {
      // Just four underscores, which means render the context right here:
      case 0 => context.value.render(context)
      // There is content, so turn it into a link to the context Thing:
      case _ => {
        val guts = processParseTree(contents, context)
        def makeWikiLink(url:String):Wikitext = {
          Wikitext("[") + guts + Wikitext("](" + url + ")")
        }
        context.value.pt match {
          case LinkType => {
            // TODO: this is evil. How should it be described instead?
            val l = LinkType.follow(context)(LinkType.get(context.value.v.first))
            l match {
              case Some(thing) => makeWikiLink(thing.toThingId)
              case None => guts
            }
          }
          case ExternalLinkType => {
            val url = ExternalLinkType.get(context.value.v.first)
            makeWikiLink(url.toExternalForm())
          }
          // TODO: we ought to show some sort of error here?
          case _ => guts
        }        
      }
    }
  }
  
  private def processParseTree(parseTree:ParsedQLText, context:ContextBase):Wikitext = {
    logContext("processParseTree " + parseTree, context)
    (Wikitext("") /: parseTree.parts) { (soFar, nextPart) =>
      soFar + (nextPart match {
        case UnQLText(t) => Wikitext(t)
        case QLExp(phrases) => contextsToWikitext(processPhrases(phrases, context))
        case QLLink(l) => linkToWikitext(l, context)
      })
    }
  }
  
  def parse = parseAll(qlText, input.text)
  
  def process:Wikitext = {
    val parseResult = parse
    parseResult match {
      case Success(result, _) => processParseTree(result, initialContext)
      case Failure(msg, next) => { Logger.warn(s"Couldn't parse qlText: $msg at ${next.pos}"); Wikitext("Couldn't parse qlText: " + msg) }
      // TODO: we should probably do something more serious in case of Error:
      case Error(msg, next) => { Logger.error("Couldn't parse qlText: " + msg); Wikitext("ERROR: Couldn't parse qlText: " + msg) }
    }
  }
  
  def processMethod:ContextBase = {
    val parseResult = parseAll(qlPhrase, input.text)
    parseResult match {
      case Success(result, _) => processPhrase(result.ops, initialContext)
      case Failure(msg, next) => { initialContext.next(ErrorValue("Syntax error: " + msg)) }
      case Error(msg, next) => { initialContext.next(ErrorValue("ERROR in parsing QL field: " + msg)) }
    }
  }
}