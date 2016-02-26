package querki.ql

import language.existentials

import scala.annotation.tailrec
import scala.util.parsing.combinator._

import models._

import querki.core.QLText
import querki.ecology._
import querki.globals._
import querki.html.QHtml
import querki.util.{DebugRenderable, UnexpectedPublicException}
import querki.values._

/**
 * This is created by and obtained from the QLEcot:
 */
private [ql] class QLProfilers(implicit val ecology:Ecology) extends EcologyMember {
  lazy val Profiler = interface[querki.tools.Profiler]  
  
  lazy val parseMethod = Profiler.createHandle("QLParser.parseMethod")
  lazy val processMethod = Profiler.createHandle("QLParser.processMethod")
  lazy val processCall = Profiler.createHandle("QLParser.processCall")
  lazy val processTextStage = Profiler.createHandle("QLParser.processTextStage")
  lazy val processNumber = Profiler.createHandle("QLParser.processNumber")
  lazy val processCallDetail = Profiler.createHandle("QLParser.call")
  lazy val processThing = Profiler.createHandle("QLParser.processThing")
  lazy val wikify = Profiler.createHandle("QLParser.wikify")
}

case class QLScope(bindings:Map[String, QValue] = Map.empty) {
  def bind(pair:(String, QValue)) = copy(bindings = bindings + pair)
  def lookup(name:String) = bindings.get(name)
}
case class QLScopes(scopes:List[QLScope] = List.empty) {
  // Add the specified binding to the current scope:
  def bind(pair:(String, QValue)) = {
    val newScopes = (scopes.head.bind(pair)) :: scopes.tail
    copy(scopes = newScopes)
  }
  
  def lookup(name:String):Option[QValue] = {
    (Option.empty[QValue] /: scopes) { (result, scope) =>
      result match {
        case Some(r) => result
        case None => scope.lookup(name)
      }
    }
  }
  
  def push = copy(scopes = QLScope() :: scopes)
  def pop = copy(scopes = scopes.tail)
}

class QLParser(val input:QLText, ci:QLContext, invOpt:Option[Invocation] = None, 
  val lexicalThing:Option[PropertyBundle] = None, val lexicalProp:Option[AnyProp] = None)(implicit val ecology:Ecology) 
  extends RegexParsers with EcologyMember 
{
  
  // Add the parser to the context, so that methods can call back into it. Note that we are treating this as essentially
  // a modification, rather than another level of depth. We also provide an initial scope for this parser:
  val initialContext = ci.copy(parser = Some(this), scopes = ci.scopes + (this -> QLScopes().push))(ci.state, ecology)
  
  val paramsOpt = invOpt.flatMap(_.paramsOpt)
  
  lazy val Basic = interface[querki.basic.Basic]
  lazy val Core = interface[querki.core.Core]
  lazy val QL = interface[querki.ql.QL]
  lazy val QLInternals = interface[QLInternals]
  
  lazy val qlProfilers = QLInternals.qlProfilers
  
  lazy val ExactlyOne = Core.ExactlyOne
  lazy val PlainTextType = Basic.PlainTextType
  
  def WarningValue = QL.WarningValue _

  // *****************************************
  //
  // Context Logging
  //
  // Crude but useful debugging of the process tree. Could stand to be beefed up when I have time
  //
  
  // A unique ID that we attach to each Stage, for understanding what's going on:
  val stageId = new java.util.concurrent.atomic.AtomicInteger()
  // Turn this to true to produce voluminous output from the QL processing pipeline
  val doLogContext = Config.getBoolean("querki.test.logContexts", false)
  def logStage(stage:QLStage, context:QLContext)(processor: => Future[QLContext]):Future[QLContext] = {
    if (doLogContext) {
      try {
        val sid = stageId.getAndIncrement
        val indent = "  " * context.depth
        QLog.info(s"$indent$sid: [[${stage.reconstructString}]] on ${context.debugRender}")
        val result = processor
        result.onComplete {
          case scala.util.Success(result) => 
            QLog.info(s"$indent$sid = ${result.debugRender}")
          case scala.util.Failure(ex) =>
            QLog.error(s"$indent$sid returned Failure!", ex)
        }
        result
      } catch {
        case th:Throwable => { 
          QLog.error(s"Exception while processing stage ${stage.reconstructString} with context ${context.debugRender}", th)
          throw th
        }
      }
    } else
      processor
  }
  
  // *****************************************
  
  // Note that we allow *one* slash in a name, but not pairs:
  val name = """[a-zA-Z_]([\w-_ ]|/?!/)*[\w]""".r
  // These two regexes are used in the unQLText production. Yes, they *could* be combined into one
  // expression, and originally were. They were split because having them together apparently does
  // something horrible to Java's Regex engine -- if you tried to feed it more than a page or so of
  // matching text (that is, with no QL expressions), it would explode with a Stack Overflow error.
  // Splitting them seems to cure that, knock on wood.
  val unQLTextRegex = """[^\[\]\"_\\]+""".r
  val partialDelimiterRegex = """(\[(?!\[)|\"(?!\")|_(?!_)|\](?!\]))+""".r
  // We don't want the RegexParser removing whitespace on our behalf. Note that we need to be
  // very careful about whitespace!
  override val whiteSpace = "".r

  // Comments are defined in here. Note that a comment is defined as everything from "//"
  // to the end of the line *or* a "]]", so that we can put them at the end of an expression.
  def qlSpace:Parser[QLSpace] = "(\\s*(//([^\n\\]]|(\\]?!\\]))*)?\\s*)*".r ^^ { QLSpace(_) }
  
  def unQLText:Parser[UnQLText] = (unQLTextRegex | 
      // You can use backslash to escape the "special" QL syntax delimiters:
      "\\\\".r ~> ("\"\"".r | "\\[\\[".r | "\\]\\]".r | "____".r | "__".r) |
      // If we haven't use the backslash for that, then just eat it as a normal character:
      "\\\\".r | 
      partialDelimiterRegex) ^^ { UnQLText(_) }
  def qlNumber[QLNumber] = "\\s*".r ~> "\\d+".r ^^ { intStr => QLNumber(java.lang.Integer.parseInt(intStr)) }
  def qlSafeName[QLSafeName] = name ^^ { QLSafeName(_) }
  def qlDisplayName[QLDisplayName] = "`" ~> "[^`]*".r <~ "`" ^^ { QLDisplayName(_) }
  def qlThingId[QLThingId] = "." ~> "\\w*".r ^^ { oid => QLThingId("." + oid) }
  def qlName:Parser[QLName] = qlBinding | qlThingId | qlSafeName | qlDisplayName
  def qlCall:Parser[QLCall] = opt("\\*\\s*".r) ~ qlName ~ opt("." ~> qlName) ~ opt("\\(\\s*".r ~> (rep1sep(qlParam, "\\s*,\\s*".r) <~ "\\s*\\)".r)) ^^ { 
    case collFlag ~ n ~ optMethod ~ optParams => QLCall(n, optMethod, optParams, collFlag) }
  // Note that the failure() here only works because we specifically disallow "]]" in a Text!
  def qlTextStage:Parser[QLTextStage] = (opt("\\*\\s*".r) <~ "\"\"") ~ qlText <~ ("\"\"" | failure("Reached the end of the QL expression, but missing the closing \"\" for a Text expression in it") ) ^^ {
    case collFlag ~ text => QLTextStage(text, collFlag) }
  def qlBinding:Parser[QLBinding] = "\\s*".r ~> (opt("+") <~ "\\$".r) ~ name ^^ { case assignOpt ~ name => QLBinding(name, assignOpt.isDefined) } 
  def qlStage:Parser[QLStage] = qlNumber | qlCall | qlTextStage
  def qlParam:Parser[QLParam] = opt(name <~ "\\s*=\\s*".r) ~ opt("!") ~ qlExp ^^ { 
    case nameOpt ~ immediateOpt ~ exp => QLParam(nameOpt, exp, immediateOpt.isDefined) 
  }
  def qlPhrase:Parser[QLPhrase] = rep1sep(qlStage, qlSpace ~ "->".r ~ qlSpace) ^^ { QLPhrase(_) }
  def qlExp:Parser[QLExp] = opt(qlSpace) ~> repsep(qlPhrase, "\\s*\\r?\\n\\s*|\\s*;\\s*".r) <~ opt(qlSpace) ^^ { QLExp(_) }
  def qlLink:Parser[QLLink] = qlText ^^ { QLLink(_) }
  def qlText:Parser[ParsedQLText] = rep(unQLText | "[[" ~> qlExp <~ "]]" | "__" ~> qlLink <~ ("__" | failure("Underscores must always be in pairs or sets of four"))) ^^ { 
    ParsedQLText(_) }
  
  /**
   * Note that the output here is nominally a new Context, but the underlying type is
   * ParsedTextType. So far, you can't *do* anything with that afterwards other than
   * render it, which just returns the already-computed Wikitext.
   */
  private def processTextStage(text:QLTextStage, context:QLContext):Future[QLContext] = {
    val ct = context.value.cType
    // For each element of the incoming context, recurse in and process the embedded Text
    // in that context. Iff the context is empty, though, just produce an empty result.
    val transformedFuts =
      if (context.isEmpty)
        Iterable.empty
      else
        context.map { elemContext =>
          processParseTree(text.contents, elemContext).map(QL.ParsedTextType(_))
        }
    
    val transformedFut = Future.sequence(transformedFuts)
    
    // TBD: the asInstanceOf here is surprising -- I would have expected transformed to come out
    // as the right type simply by type signature. Can we get rid of it?
    transformedFut.map(transformed => context.next(ct.makePropValue(transformed.asInstanceOf[ct.implType], QL.ParsedTextType)))
  }
  
  private def processCall(call:QLCall, context:QLContext, isParam:Boolean):Future[QLContext] = {
    
    def lookupMethod():Future[Option[Thing]] = {
      call.methodName match {
        case Some(qlName) => {
          qlName match {
            case binding:QLBinding => {
              processBinding(binding, context, isParam) map { resolvedBinding =>
                if (resolvedBinding.value.matchesType(Core.LinkType)) {
                  for {
                    // TODO: for now, we don't cope with multiple resolved results here. I'm not even
                    // sure why you'd ever want that:
                    oid <- resolvedBinding.value.rawList(Core.LinkType).headOption
                    thing <- context.state.anything(oid)
                  }
                    yield thing
                } else {
                  None
                }                
              }
            }
            case tid:QLThingId => Future.successful(context.state.anything(ThingId(tid.name)))
            case _ => Future.successful(context.state.anythingByName(qlName.name))
          }
        }
        case None => Future.successful(None)
      }
    }
    
    def processThing(t:Thing, context:QLContext):Future[QLContext] = qlProfilers.processThing.profile {
      // If there are parameters to the call, they are a collection of phrases. If any are marked immediate
      // (prefixed with "!"), they gets processed *now*, rather than meta-programmed later:
      val paramsFOpt:Option[Seq[Future[QLParam]]] = call.params map { params =>
        params.map { param =>
          if (param.immediate) {
            processExp(param.exp, context).map { processed =>
              param.copy(resolved = Some(processed))
            }
          } else
            Future.successful(param)
        }
      }
      val paramsFut:Future[Option[Seq[QLParam]]] = paramsFOpt match {
        case Some(paramsSeq) => {
          Future.sequence(paramsSeq).map(Some(_))
        }
        case None => Future.successful(None)
      }
      paramsFut.flatMap { params =>
        lookupMethod().flatMap {
          case Some(method) => {
            val definingContext = context.next(Core.ExactlyOne(Core.LinkType(t.id)))
            qlProfilers.processCallDetail.profileAs(" " + call.name.name) {
              method.qlApplyTop(InvocationImpl(t, method, context.withCall(call, method), Some(definingContext), params), method)
            }
          }
          case None => {
            qlProfilers.processCallDetail.profileAs(" " + call.name.name) {
              val inv = InvocationImpl(t, t, context.withCall(call, t), None, params)
              t.qlApplyTop(inv, t)
            }
          }
        }
      }
    }
    
    def handleThing(tOpt:Option[Thing], context:QLContext) = {
      tOpt.map { t =>
        processThing(t, context)
      }.getOrElse(Future.successful(context.next(Core.ExactlyOne(QL.UnknownNameType(call.name.name)))))
    }
    
    call.name match {
      case binding:QLBinding => { 
        processBinding(binding, context, isParam).flatMap { resolvedBinding =>  
          if (resolvedBinding.value.matchesType(Core.LinkType)) {
            val resultFuts = for {
              oid <- resolvedBinding.value.rawList(Core.LinkType)
              thing <- context.state.anything(oid)
              fut = processThing(thing, context)
            }
              yield fut
              
            val resultFut = Future.sequence(resultFuts)
            resultFut.map(results => resolvedBinding.concat(results))
          } else {
            Future.successful(resolvedBinding)
          }
        }
      }
      case tid:QLThingId => {
        val tOpt = context.state.anything(ThingId(tid.name))
        handleThing(tOpt, context)
      }
      case _ => {
        val tOpt = context.state.anythingByName(call.name.name)
        handleThing(tOpt, context)
      }
    }
  }
  
  def warningFut(context:QLContext, msg:String):Future[QLContext] = {
    Future.successful(context.next(WarningValue(msg)))
  }
  
  private def processNormalBinding(binding:QLBinding, context:QLContext, isParam:Boolean, resolvingParser:QLParser):Future[QLContext] = {
    val scopes = context.scopes(this)
    val boundOpt = scopes.lookup(binding.name)
    if (binding.assign) {
      if (boundOpt.isDefined) {
        warningFut(context, s"Attempting to reassign ${"$" + binding.name} -- you may only say ${"+$" + binding.name} once")
      } else {
        // Okay -- we are legally attempting to bind this name to the received value:
        val newScopes = scopes.bind((binding.name -> context.value))
        val newContext = context.withScopes(this, newScopes)
        fut(newContext)
      }
    } else {
      boundOpt match {
        // Fetching an already-bound value
        case Some(bound) => fut(context.next(bound))
        case None => {
          // It's not bound lexically. Does it exist in the page's query parameters?
          context.requestParams.get(binding.name) match {
            // Yep -- process that
            case Some(phrase) => processPhrase(phrase.ops, context, true)
            // Okay, this really seems to be an unbound value, so give an error:
            case None => warningFut(context, s"Didn't find a value for ${binding.name}")
          }          
        }
      }
    }
  }
  
  private def processInternalBinding(binding:QLBinding, context:QLContext, isParam:Boolean, resolvingParser:QLParser):Future[QLContext] = {
    if (binding.name == "_context") {
      Future.successful(resolvingParser.initialContext)
    } else if (binding.name == "_defining") {
      // If this expression was invoked with a defining context, produce that:
      val found = for {
        inv <- invOpt
        defining <- inv.definingContext
      }
        yield defining
        
      Future.successful(found.getOrElse(context.next(Core.emptyOpt(Core.LinkType))))
    } else {
      try {
        val rawParamNum = Integer.parseInt(binding.name.substring(1))
        val paramNum = rawParamNum - 1
        val resultOpt = for {
          params <- resolvingParser.paramsOpt
          param = params(paramNum)
        }
          yield {
            param.resolved match {
              // This parameter was resolved at the call site ("!")
              case Some(resolved) => Future.successful(resolved)
              // Normal parameter -- resolve it here:
              // TODO: the isParam parameter we're using here is a horrible, opaque hack.
              // Disentangle it, and figure out a better approach:
              case None => processExp(param.exp, context, true)
            }
          }
        
        resultOpt.getOrElse(warningFut(context,"No parameters passed in"))
      } catch {
        case ex:NumberFormatException => warningFut(context, "$" + binding.name + " is not a valid bound name")
        case ex:IndexOutOfBoundsException => warningFut(context, "Not enough parameters passed in")
      }
    }
  }
  
  private def processBinding(binding:QLBinding, context:QLContext, isParam:Boolean):Future[QLContext] = {
    // What is this? The thing is, bindings are complicated. Iff we are in a subparser, *and* we are resolving
    // the parameters from that subparser, then we need to be resolving the bindings against the *parent*, not
    // against this parser. That is, "myMethod($_1)"'s parameter is parameter 1 from the *caller*. Our normal
    // resolution would be against the *callee*.
    val resolvingParser = {
      if (invOpt.isDefined && isParam)
        invOpt.flatMap(_.context.parser).get
      else
        this
    }
  
    if (binding.name.startsWith("_"))
      processInternalBinding(binding, context, isParam, resolvingParser)
    else
      processNormalBinding(binding, context, isParam, resolvingParser)
  }
  
  private def processNumber(num:QLNumber, context:QLContext):QLContext = {
    context.next(Core.ExactlyOne(Core.IntType(num.n)))
  }
  
  private def processStage(stage:QLStage, contextIn:QLContext, isParam:Boolean):Future[QLContext] = {
    logStage(stage, contextIn) {
      val context = 
        if (contextIn.depth > contextIn.maxDepth)
          contextIn.next(WarningValue("Too many levels of calls -- you can only have up to " + contextIn.maxDepth + " calls in a phrase."))
        else if (stage.useCollection) 
          contextIn.asCollection
        else if (stage.clearUseCollection)
          contextIn.clearAsCollection
        else 
          contextIn
      stage match {
        case name:QLCall => processCall(name, context, isParam)
        case subText:QLTextStage => processTextStage(subText, context)
        case num:QLNumber => Future.successful(processNumber(num, context))
      }
    }
  }
  
  def processPhrase(ops:Seq[QLStage], context:QLContext, isParam:Boolean = false):Future[QLContext] = {
    if (context.isCut)
      // Setting the "cut" flag means that we're just stopping processing here. Usually
      // used for warnings:
      Future.successful(context)
    else {
      if (ops.isEmpty)
        Future.successful(context)
      else
        processStage(ops.head, context, isParam) flatMap { next => processPhrase(ops.tail, next, isParam) }
    }
  }
  
  /**
   * Wraps the given function in a new scope, which will get popped once the operation completes.
   */
  def withScope(contextIn:QLContext, f: QLContext => Future[QLContext]):Future[QLContext] = {
    val newScopes = contextIn.scopes(this).push
    val allScopes = contextIn.scopes + (this -> newScopes)
    val context = contextIn.copy(scopes = allScopes)(contextIn.state, contextIn.ecology)
    f(context).map { contextOut =>
      contextOut.withScopes(this, contextOut.scopes(this).pop)
    }
  }
  
  /**
   * Variant of processPhrase that treats the phrase as its own distinct scope, which will be discarded when
   * we reach the end of processing.
   */
  def processPhraseAsScope(ops:Seq[QLStage], contextIn:QLContext, isParam:Boolean = false):Future[QLContext] = {
    withScope(contextIn, processPhrase(ops, _, isParam))
  }
  
  private def processPhrases(phrases:Seq[QLPhrase], context:QLContext, isParam:Boolean = false):Seq[Future[QLContext]] = {
    val (lastContext, futs) = ((fut(context), Seq.empty[Future[QLContext]]) /: phrases) { (inp, phrase) =>
      val (prevContextFut, results) = inp
      val nextContext = prevContextFut.flatMap { prevContext =>
        // We process the next phrase *mainly* with the original context, but we need to swap in
        // any accumulated bindings:
        processPhrase(phrase.ops, context.withScopes(this, prevContext.scopes.get(this).getOrElse(QLScopes())), isParam)
      }
      val fixedContext = phrase.ops.last match {
        case QLCall(QLBinding(n, assign), _, _, _) if (assign) => {
          // The last Stage of this Phrase was an assignment; in this very special case, we
          // suppress the output *unless* it was an error:
          nextContext.map { c =>
            if (c.isError)
              c
            else
              c.next(Core.QNone) 
          }
        }
        case _ => nextContext
      }
      (fixedContext, results :+ fixedContext)
    }
    futs
  }
  
  def processExp(exp:QLExp, context:QLContext, isParam:Boolean = false):Future[QLContext] = {
    // TODO: for the moment, we're only coping with the final resulting Context here. That is
    // *usually* what you want, but certainly more restrictive than the official semantics of QL.
    // Figure out how to make the callers of this cope with Seq[Future[QLContext]] (or Future[Seq[QLContext]]),
    // and adjust this to produce that.
    processPhrases(exp.phrases, context, isParam).last
  }
  
  def processExpAsScope(exp:QLExp, context:QLContext):Future[QLContext] = {
    withScope(context, processExp(exp, _))
  }

  def contextsToWikitext(contexts:Seq[Future[QLContext]], insertNewlines:Boolean = false):Future[Wikitext] = {
    // Each of the received contexts gets treated separately.
    // TODO: this will almost certainly change once we introduce local bindings! At that point, the
    // phrases in a given expression need to be closely linked!
    val wikified = contexts.map { _.flatMap( context => context.value.wikify(context.parent)) }
    Future.fold(wikified)(Wikitext(""))(_.+(_, insertNewlines))
  }
  
  /**
   * This deals with QText that contains "__stuff__" or "____", both of which render as
   * the value of the received context.
   * 
   * The key notion here is that the wikify() method takes an optional "displayOpt" parameter.
   * If specified, this says how to display the passed-in value. It doesn't make sense for all
   * Types, but it is always *syntactically* legal, and is provided as a tool for any Type that
   * wants to avail itself of this notion.
   */
  private def linkToWikitext(contents:ParsedQLText, context:QLContext):Future[Wikitext] = {
    contents.parts.length match {
      // Just four underscores, which means we don't do anything special for the display:
      case 0 => context.value.wikify(context, None)
      // There is content, so pass that down as the display:
      case _ => processParseTree(contents, context).flatMap(guts => context.value.wikify(context, Some(guts)))
    }
  }
  
  private def processParseTree(parseTree:ParsedQLText, context:QLContext):Future[Wikitext] = {
    // Note that the parts at this level are independent -- the context doesn't carry over -- which
    // makes this relatively easy. We just compute each piece, and then fold them together:
    val futures = parseTree.parts.map { part =>
      part match {
        case UnQLText(t) => Future.successful(Wikitext(t))
        case QLExp(phrases) => contextsToWikitext(processPhrases(phrases, context))
        case QLLink(l) => linkToWikitext(l, context)
      }          
    }
    Future.fold(futures)(Wikitext(""))(_ + _)
  }
  
  def parse = parseAll(qlText, input.text)
  
  def consumeReader[T](reader:scala.util.parsing.input.Reader[T]):String = {
    reader.first.toString + (if (reader.atEnd) "" else consumeReader(reader.rest))
  }
  
  // TODO: this really shouldn't be showing raw HTML. Redo this properly as Wikitext:
  def renderError(msg:String, reader:scala.util.parsing.input.Reader[_]):Future[Wikitext] = {
    val pos = reader.pos
    val propStr = initialContext.getProp match {
      case Some(prop) => " " + prop.displayName + ", "
      case None => ""
    }
    val escapedMsg = s"<b>Syntax error in $propStr line ${pos.line}:</b> " + scala.xml.Utility.escape(msg)
    val escapedError = scala.xml.Utility.escape(pos.longString)
    Future.successful(HtmlWikitext(QHtml(
        "<p>" + escapedMsg + ":<p>\n" +
        "<pre>" + escapedError + "</pre>\n")))
  }
  
  def wikiFut(str:String):Future[Wikitext] =
    Future.successful(Wikitext(str))
  
  def process:Future[Wikitext] = {
    try {
      val parseResult = parse
      parseResult match {
        case Success(result, _) => {
          processParseTree(result, initialContext)
            .recoverWith {
              case ex:PublicException => WarningValue(ex.display(initialContext.requestOpt)).wikify(initialContext)
            }
        }
        case Failure(msg, next) => { renderError(msg, next) }
        // TODO: we should probably do something more serious in case of Error:
        case Error(msg, next) => { QLog.error("Couldn't parse qlText: " + msg); renderError(msg, next) }
      }
    } catch {
      case overflow:java.lang.StackOverflowError => {
        QLog.error("Stack overflow error while trying to parse this QLText:\n" + input.text)
        overflow.printStackTrace()
        wikiFut("We're sorry -- this thing is apparently more complex than Querki can currently cope with. Please contact Justin: this is a bug we need to fix.")
      }
      case error:Exception => {
        QLog.error("Exception during QL Processing: " + error, error)
        wikiFut("We're sorry -- there was an error while trying to display this thing.")
      }
      case error:Throwable => {
        QLog.error("Throwable during QL Processing: " + error, error)
        wikiFut("We're sorry -- there was a serious error while trying to display this thing.")
      }
    }
  }
  
  def parsePhrase():Option[QLPhrase] = {
    val parseResult = parseAll(qlPhrase, input.text)
    parseResult match {
      case Success(result, _) => Some(result)
      case _ => None
    }
  }
  
  private[ql] def processMethod:Future[QLContext] = {
    val parseResult = qlProfilers.parseMethod.profile { parseAll(qlExp, input.text) }
    parseResult match {
      case Success(result, _) => 
        qlProfilers.processMethod.profile { 
          processExp(result, initialContext, false)
          .recoverWith {
            case ex:PublicException => warningFut(initialContext, ex.display(initialContext.requestOpt))
          }
        }
      case Failure(msg, next) => { renderError(msg, next).map(err => initialContext.next(QL.WikitextValue(err))) }
      case Error(msg, next) => { renderError(msg, next).map(err => initialContext.next(QL.WikitextValue(err))) }
    }
  }
}