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
  
  lazy val parse = Profiler.createHandle("QLParser.parse")
  lazy val processMethod = Profiler.createHandle("QLParser.processMethod")
  lazy val processCall = Profiler.createHandle("QLParser.processCall")
  lazy val processTextStage = Profiler.createHandle("QLParser.processTextStage")
  lazy val processNumber = Profiler.createHandle("QLParser.processNumber")
  lazy val processCallDetail = Profiler.createHandle("QLParser.call")
  lazy val processThing = Profiler.createHandle("QLParser.processThing")
  lazy val wikify = Profiler.createHandle("QLParser.wikify")
}

sealed trait QLParseResult[T <: QLParseResultVal]
case class QLParseSuccess[T <: QLParseResultVal](result:T) extends QLParseResult[T]
case class QLParseFailure[T <: QLParseResultVal](msgFut:Future[Wikitext]) extends QLParseResult[T]

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

class QLParser(
    val input:QLText, ci:QLContext, invOpt:Option[Invocation] = None, 
    val lexicalThing:Option[PropertyBundle] = None, val lexicalProp:Option[AnyProp] = None,
    val initialBindings:Option[Map[String, QValue]] = None)
  (implicit val ecology:Ecology) 
  extends RegexParsers with EcologyMember 
{
  
  lazy val initialScopes = initialBindings match {
    case Some(bindings) => QLScopes(List(QLScope(bindings)))
    case None => QLScopes()
  }
  // Add the parser to the context, so that methods can call back into it. Note that we are treating this as essentially
  // a modification, rather than another level of depth. We also provide an initial scope for this parser:
  val initialContext = ci.copy(parser = Some(this), scopes = ci.scopes + (this -> initialScopes.push))(ci.state, ecology)
  
  /**
   * Create a new parser, that includes the given bindings. This is intended mainly for "meta" functions,
   * which want to fix some bindings and then call processExp().
   */
  def withBindings(bindings: Map[String, QValue]):QLParser = {
    new QLParser(input, ci, invOpt, lexicalThing, lexicalProp, Some(initialBindings.getOrElse(Map.empty) ++ bindings))
  }
  
  val paramsOpt = invOpt.flatMap(_.paramsOpt)
  
  lazy val Basic = interface[querki.basic.Basic]
  lazy val Core = interface[querki.core.Core]
  lazy val QL = interface[querki.ql.QL]
  lazy val QLInternals = interface[QLInternals]
  lazy val Tags = interface[querki.tags.Tags]
  
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
  
  final val name = querki.core.nameRegex
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
  def qlNumber[QLNumber] = "\\s*".r ~> "\\-?\\d+".r ^^ { intStr => QLNumber(java.lang.Long.parseLong(intStr)) }
  def qlSafeName[QLSafeName] = name ^^ { QLSafeName(_) }
  def qlDisplayName[QLDisplayName] = "`" ~> "[^`]*".r <~ "`" ^^ { QLDisplayName(_) }
  def qlThingId[QLThingId] = "." ~> "\\w*".r ^^ { oid => QLThingId("." + oid) }
  def qlName:Parser[QLName] = qlDef | qlBindingDef | qlBinding | qlThingId | qlSafeName | qlDisplayName
  def qlOp:Parser[String] = ("&" | "|")
  def qlUnOp:Parser[String] = ("!")
  def qlCall:Parser[QLCall] = opt("\\*\\s*".r) ~ qlName ~ opt("." ~> qlName) ~ opt(qlParamList) ^^ { 
    case collFlag ~ n ~ optMethod ~ optParams => QLCall(n, optMethod, optParams, collFlag) }
  // Note that the failure() here only works because we specifically disallow "]]" in a Text!
  def qlTextStage:Parser[QLTextStage] = (opt("\\*\\s*".r) <~ "\"\"") ~ qlText <~ ("\"\"" | failure("Reached the end of the QL expression, but missing the closing \"\" for a Text expression in it") ) ^^ {
    case collFlag ~ text => QLTextStage(text, collFlag) }
  def qlExpStage:Parser[QLStage] = "(" ~> qlExp <~ ")" ^^ { QLExpStage(_) }
  def qlDef:Parser[QLBindingDef] = "\\s*_def\\s+\\$".r ~> name ~ opt("(" ~> rep1sep("$" ~> name, "\\s*,\\s*".r) <~ ")") ~ ("\\s+=\\s+".r ~> qlPhrase) ^^ {
    case name ~ params ~ phrase => QLBindingDef(name, Some(phrase), params)
  }
  def qlTextBlockLiteral: Parser[QLTextBlockLiteral] = "\\s*".r ~ "```" ~> "(?s)(?:(?!```).)*".r <~ "```" ^^ {
    block => QLTextBlockLiteral(block) 
  }
  def qlBinding:Parser[QLBinding] = "\\s*\\$".r ~> name ^^ { 
    case name => QLBinding(name) 
  }
  def qlBindingDef:Parser[QLBindingDef] = "\\s*\\+\\$".r ~> name ~ opt("\\(\\s*".r ~> (repsep(qlParam, "\\s*,\\s*".r) <~ "\\s*\\)".r)) ^^ { 
    case name ~ params => QLBindingDef(name, None, None) 
  }
  def qlBasicStage:Parser[QLStage] = qlTextBlockLiteral | qlNumber | qlCall | qlTextStage | qlList | qlExpStage
  def qlUnOpStage:Parser[QLStage] = opt(qlUnOp <~ "\\s*".r) ~ qlBasicStage ^^ {
    case unop ~ guts => {
      unop match {
        case Some(op) => {
          val funcName = op match {
            case "!" => "_not"
            case _ => throw new Exception(s"qlUnaryOperation got unknown operator $op")
          }
          QLCall(QLSafeName(funcName), None,
              Some(Seq(QLParam(None, QLExp(Seq(QLPhrase(Seq(guts))))))),
              None)
        }
        case None => guts
      }
    }
  }
  def qlStage:Parser[QLStage] = qlUnOpStage ~ opt("\\s+".r ~> qlOp ~ ("\\s+".r ~> qlUnOpStage)) ^^ {
    case left ~ operation => {
      operation match {
        case Some(op ~ right) => {
          val funcName = op match {
            case "&" => "_and"
            case "|" => "_or"
            case _ => throw new Exception(s"qlBinaryOperation got unknown operator $op")
          }
          QLCall(QLSafeName(funcName), None, 
              Some(Seq(QLParam(None, QLExp(Seq(QLPhrase(Seq(left))))),
                       QLParam(None, QLExp(Seq(QLPhrase(Seq(right))))))),
              None)
        }
        case None => left
      }
    }
  }
  def qlParamList:Parser[Seq[QLParam]] = qlEmptyParamList | qlRealParamList
  def qlEmptyParamList:Parser[Seq[QLParam]] = "\\(\\s*\\)".r ^^ { case _ => Seq.empty }
  def qlRealParamList:Parser[Seq[QLParam]] = "\\(\\s*".r ~> (rep1sep(qlParam, "\\s*,\\s*".r) <~ "\\s*\\)".r)
  def qlParam:Parser[QLParam] = opt(name <~ "\\s*=\\s*".r) ~ opt("~!") ~ qlExp ^^ { 
    case nameOpt ~ immediateOpt ~ exp => QLParam(nameOpt, exp, immediateOpt.isDefined) 
  }
  def qlPhrase:Parser[QLPhrase] = rep1sep(qlStage, qlSpace ~ "->".r ~ qlSpace) ^^ { QLPhrase(_) }
  // Phrases are separated either by qlSpace (including comments) or semicolons
  def qlExp:Parser[QLExp] = opt(qlSpace) ~> repsep(qlPhrase, opt(qlSpace) ~ opt(";") ~ qlSpace) <~ opt(qlSpace) ^^ { QLExp(_) }
  def qlLink:Parser[QLLink] = qlText ^^ { QLLink(_) }
  def qlText:Parser[ParsedQLText] = rep(unQLText | "[[" ~> qlExp <~ "]]" | "__" ~> qlLink <~ ("__" | failure("Underscores must always be in pairs or sets of four"))) ^^ { 
    ParsedQLText(_) }
  def qlList:Parser[QLCall] = "<" ~> rep1sep(qlExp, qlSpace ~ "," ~ qlSpace) <~ qlSpace ~ ">" ^^ 
    // We've found that _concat already works well for this, so simply desugar to that:
    // { QLListLiteral(_) }
    { params => QLCall(QLSafeName("_concat"), None, Some(params.map(param => QLParam(None, param, false))), None) }
  
  /**
   * Note that the output here is nominally a new Context, but the underlying type is
   * ParsedTextType. So far, you can't *do* anything with that afterwards other than
   * render it, which just returns the already-computed Wikitext.
   */
  private def processTextStage(text:QLTextStage, context:QLContext):Future[QLContext] = {
    val ct = context.value.cType
    // For each element of the incoming context, recurse in and process the embedded Text
    // in that context. Iff the context is empty, though, just produce an empty result *unless*
    // they have asked for the full collection.
    val transformedFuts =
      if (context.isEmpty && text.collFlag.isEmpty)
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
              processBinding(binding, context, isParam, call) map { resolvedBinding =>
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
    
    call.name match {
      case binding:QLBinding => { 
        processBinding(binding, context, isParam, call).flatMap { resolvedBinding =>  
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
      case bindingDef:QLBindingDef => {
        processBindingDef(bindingDef, context)
      }
      case tid:QLThingId => {
        // Note: this is structured to fix QI.7w4g8tv: cope if the tid *looks* like an OID, but
        // doesn't parse right:
        ThingId.parseOpt(tid.name).map { thingId =>
          context.state.anything(thingId) match {
            case Some(t) => processThing(t, context)
            case None => Future.failed(new PublicException("QL.unknownThingId", tid.reconstructString))
          }
        }.getOrElse(Future.failed(new PublicException("QL.unknownThingId", tid.reconstructString)))
      }
      case _ => {
        // Looking up a Thing by name, which is the most common situation:
        context.state.anythingByName(call.name.name) match {
          // Normal case: we found this Thing by name:
          case Some(t) => processThing(t, context)
          // We *didn't* find the Thing...
          case None => {
            // Note: before QI.9v5kelv, we distinguished depending on whether call.name.name was a known Tag
            // (in which case we returned UnknownNameType) or not (in which case we raised the
            // "QL.unknownName" error). We now only do the former -- the latter has simply proven to be
            // annoying without much benefit. (We also allowed backtick-delimited names even if they were
            // unknown, which was *way* too subtle.)
            Future.successful(context.next(Core.ExactlyOne(QL.UnknownNameType(call.name.name))))
          }
        }
      }
    }
  }
  
  def warningFut(context:QLContext, msg:String):Future[QLContext] = {
    Future.successful(context.next(WarningValue(msg)))
  }
  
  private def processBindingDef(binding:QLBindingDef, context:QLContext):Future[QLContext] = {
    context.scopes.get(this).map { scopes =>
      val boundOpt = scopes.lookup(binding.name)
      if (boundOpt.isDefined) {
        warningFut(context, s"Attempting to reassign ${"$" + binding.name} -- you may only say ${"+$" + binding.name} once")
      } else {
        // Okay -- we are legally attempting to bind this name to the received value:
        val newScopes:QLScopes =
          binding.func match {
            case Some(func) => {
              // We're binding a local function, so we ignore the context and instead just use that:
              val closure = QLClosure(QLExp(Seq(func)), binding.params)
              val v = ExactlyOne(QL.ClosureType(closure))
              scopes.bind((binding.name -> v))
            } 
            // Ordinary binding, so we just bind the current context to this name:
            case _ => scopes.bind((binding.name -> context.value))
          }
        val newContext = context.withScopes(this, newScopes)
        fut(newContext)
      }
    }.getOrElse {
      QLog.error(s"Failed to find the scope for binding $binding!")
      warningFut(context, s"Internal error while trying to parse!") 
    }
  }
  
  private def processNormalBinding(binding:QLBinding, context:QLContext, isParam:Boolean, resolvingParser:QLParser, call:QLCall):Future[QLContext] = {
    val scopes = context.scopes(this)
    val boundOpt = scopes.lookup(binding.name)
    boundOpt match {
      // Fetching an already-bound value
      case Some(bound) => {
        if (bound.pType == QL.ClosureType) {
          // We're invoking a local function, so process that:
          val closure = bound.firstAs(QL.ClosureType).get
          closure.params match {
            case Some(formals) => {
              // There are formal parameters for the function, so we need to bind those to the actuals:
              val actuals = call.params.getOrElse(throw new Exception(s"Locally-defined function ${binding.name} requires parameters."))
              // The number of params must match exactly, since we don't have optional params yet:
              if (formals.length != actuals.length)
                throw new Exception(s"Locally-defined function ${binding.name} requires ${formals.length} parameters.")
              val actualVs = actuals.map { actual => ExactlyOne(QL.ClosureType(QLClosure(actual.exp, None))) }
              val pairs = formals zip actualVs
              withScope(context, { scopedContext =>
                val newScopes = (scopedContext.scopes(this) /: pairs) { (scopes, pair) =>
                  scopes.bind(pair)
                }
                processExp(closure.exp, scopedContext.withScopes(this, newScopes))
              })
            }
            // An ordinary closure value, which just gets evaluated with the context:
            case None => processExp(closure.exp, context)
          }
        } else {
          // Normal value -- just stick it in:
          fut(context.next(bound))
        }
      }
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
  
  private def processInternalBinding(binding:QLBinding, context:QLContext, isParam:Boolean, resolvingParser:QLParser):Future[QLContext] = {
    if (binding.name == "_context") {
      val scopes = context.scopes(this)
      Future.successful(resolvingParser.initialContext.withScopes(this, scopes))
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
  
  private def processBinding(binding:QLBinding, context:QLContext, isParam:Boolean, call:QLCall):Future[QLContext] = {
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
      processNormalBinding(binding, context, isParam, resolvingParser, call)
  }
  
  private def processNumber(num:QLNumber, context:QLContext):QLContext = {
    if (num.n >= Integer.MAX_VALUE || num.n <= Integer.MIN_VALUE)
      context.next(Core.ExactlyOne(Core.LongType(num.n)))
    else
      context.next(Core.ExactlyOne(Core.IntType(num.n.toInt)))
  }
  
  // TODO: this is now obsolete, since List Literals now desugar to _concat, so I think it can
  // be deleted:
  private def processListLiteral(list:QLListLiteral, context:QLContext):Future[QLContext] = {
    // Process all of the list elements, in parallel:
    val futs:Seq[Future[Seq[QLContext]]] = list.exps.map(processExpAll(_, context))
    val fut:Future[Seq[QLContext]] = Future.sequence(futs).map(_.flatten)
    // Extract all the elements, and merge them into one list
    fut.map { contexts =>
      val qvs = contexts.map(_.value)
      val pt = qvs.headOption.map(_.pType).getOrElse(Core.UnknownType)
      val elems = qvs.flatMap(_.elems)
      val qv = Core.QList.makePropValue(elems, pt)
      context.next(qv)
    }
  }
  
  private def processTextBlockLiteral(contextIn: QLContext, block: QLTextBlockLiteral): Future[QLContext] = {
    fut(contextIn.next(QL.WikitextValue(Wikitext(s"```${block.text}```"))))
  }
  
  private def processStage(stage:QLStage, contextIn:QLContext, isParam:Boolean):Future[QLContext] = {
    logStage(stage, contextIn) {
      if (contextIn.depth > contextIn.maxDepth) {
        Future.successful(
          contextIn.next(WarningValue("Too many levels of calls -- you can only have up to " + contextIn.maxDepth + " calls in a phrase.")))
      } else {
        val context =
          if (stage.useCollection)
            contextIn.asCollection
          else if (stage.clearUseCollection)
            contextIn.clearAsCollection
          else
            contextIn
        stage match {
          case name: QLCall => processCall(name, context, isParam)
          case subText: QLTextStage => processTextStage(subText, context)
          case num: QLNumber => Future.successful(processNumber(num, context))
          case list: QLListLiteral => processListLiteral(list, context)
          case QLExpStage(exp) => processExp(exp, context)
          case block: QLTextBlockLiteral => processTextBlockLiteral(contextIn, block)
        }
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
    val newScopes = contextIn.scopes.get(this).getOrElse(QLScopes()).push
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
  
  private def processPhrases(phrases:Seq[QLPhrase], context:QLContext, isParam:Boolean = false):Future[Seq[QLContext]] = {
    val defaultScopes = context.scopes.get(this).getOrElse(QLScopes())
    (fut(Seq.empty[QLContext]) /: phrases) { (prev, phrase) =>
      prev.flatMap { contexts =>
        val prevContextOpt = contexts.lastOption
        prevContextOpt match {
          case Some(last) if (last.isError) => {
            // We've already gotten an error, so just ignore everything else and return what
            // we had up until there:
            fut(contexts)
          }
          case _ => {
            // Normal situation -- process the next context
            // Pull in the accumulated bindings from the last context, if there are any:
            val prevScopes = prevContextOpt.flatMap(_.scopes.get(this)).getOrElse(defaultScopes)
            // Actually process this phrase:
            val processedFut = processPhrase(phrase.ops, context.withScopes(this, prevScopes), isParam)
            processedFut.map { processed =>
              val thisContext = phrase.ops.last match {
                case QLCall(QLBindingDef(n, _, _), _, _, _) => {
                  // The last Stage of this Phrase was an assignment; in this very special case, we
                  // suppress the output *unless* it was an error:
                  if (processed.isError)
                    processed
                  else
                    processed.next(Core.QNone) 
                }
                // Normal result: just return what we just processed:
                case _ => processed
              }
              // Tack it onto the list, and we're ready to go...
              contexts :+ thisContext
            }
          }
        }
      }
    }
  }
  
  def processExpAll(exp:QLExp, context:QLContext, isParam:Boolean = false):Future[Seq[QLContext]] = {
    processPhrases(exp.phrases, context, isParam)
  }
  
  def processExp(exp:QLExp, context:QLContext, isParam:Boolean = false):Future[QLContext] = {
    // TODO: for the moment, we're only coping with the final resulting Context here. That is
    // *usually* what you want, but certainly more restrictive than the official semantics of QL.
    // Figure out how to make the callers of this cope with Seq[Future[QLContext]] (or Future[Seq[QLContext]]),
    // and adjust this to produce that.
    processExpAll(exp, context, isParam).map { contexts =>
      if (contexts.isEmpty)
        // This probably means that the expression was completely empty -- for example, an
        // empty parameter. So we treat this as simply empty.
        context.next(EmptyValue.untyped)
      else
        contexts.last
    }
  }
  
  def processExpAsScope(exp:QLExp, context:QLContext):Future[QLContext] = {
    withScope(context, processExp(exp, _))
  }

  // Note that the returned Future here is guaranteed to be successful -- Exceptions are already rendered.
  def contextsToWikitext(contextsFut:Future[Seq[QLContext]], insertNewlines:Boolean = false):Future[Wikitext] = {
    // Wikify each of them:
    val wikified = contextsFut.flatMap { contexts =>
      Future.sequence(contexts.map(context => context.value.wikify(context.parent)))
    }
    // And merge them together:
    wikified
      .map { contexts => (Wikitext("") /: contexts) (_.+(_, insertNewlines)) }
      // Iff the Future contains an Exception, render that here. This is how we display errors in-place,
      // instead of having them propagate out to overwhelm the page:
      .recoverWith {
        case ex:PublicException => warningFut(initialContext, ex.display(initialContext.requestOpt)).flatMap(_.value.wikify(initialContext))
      }
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
  
  /**
   * Wrapper around all text parsing. Deals with both profiling and memoizing the result.
   */
  private def parseMemoized[T <: QLParseResultVal](parser:Parser[T]):QLParseResult[T] = {
    qlProfilers.parse.profile { 
      input.memoizedParse {
        val result = parseAll(parser, input.text)
        result match {
          case Success(result, _) => QLParseSuccess(result)
          case Failure(msg, next) => QLParseFailure(renderError(msg, next))
          // TODO: we should probably do something more serious in case of Error:
          case Error(msg, next) => QLParseFailure { QLog.error("Couldn't parse qlText: " + msg); renderError(msg, next) }
        }
      }
    }
  }
  
  def parse = parseMemoized(qlText)
  
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
        case QLParseSuccess(result) => {
          processParseTree(result, initialContext)
            .recoverWith {
              case ex:PublicException => WarningValue(ex.display(initialContext.requestOpt)).wikify(initialContext)
            }
        }
        case QLParseFailure(msgFut) => msgFut
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
    val parseResult = parseMemoized(qlPhrase)
    parseResult match {
      case QLParseSuccess(result) => Some(result)
      case _ => None
    }
  }
  
  private[ql] def processMethodToWikitext:Future[Wikitext] = {
    val parseResult = parseMemoized(qlExp)
    parseResult match {
      case QLParseSuccess(result) => 
        qlProfilers.processMethod.profile { 
          contextsToWikitext(processExpAll(result, initialContext, false), false)
        }
      case QLParseFailure(msgFut) => { msgFut.flatMap(err => initialContext.next(QL.WikitextValue(err)).value.wikify(initialContext)) }
    }
  }
  
  private[ql] def processMethod:Future[QLContext] = {
    val parseResult = parseMemoized(qlExp)
    parseResult match {
      case QLParseSuccess(result) => 
        qlProfilers.processMethod.profile { 
          processExp(result, initialContext, false)
          .recoverWith {
            case ex:PublicException => warningFut(initialContext, ex.display(initialContext.requestOpt))
          }
        }
      case QLParseFailure(msgFut) => { msgFut.map(err => initialContext.next(QL.WikitextValue(err))) }
    }
  }
}
