package querki.text

import querki.globals._

import querki.ecology._

import models.Wikitext

import querki.core.QLText
import querki.ql.{QLCall, QLParam, QLExp}
import querki.values.{QFut, QLContext}

object MOIDs extends EcotIds(23) {
  val PluralizeOID = sysId(54)
  val JoinMethodOID = sysId(65)
  
  val MatchCaseOID = moid(1)
  val SubstringOID = moid(2)
  val TextLengthOID = moid(3)
}

class TextEcot(e:Ecology) extends QuerkiEcot(e) with querki.core.MethodDefs {
  import MOIDs._
  
  lazy val QL = interface[querki.ql.QL]
  
  val TextTag = "Text"

  /***********************************************
   * FUNCTIONS
   ***********************************************/
  
  // TODO: this should be rewritten in QL
  lazy val PluralizeMethod = new InternalMethod(PluralizeOID,
    toProps(
      setName("_pluralize"),
      Categories(TextTag),
      Summary("Produces the right word depending on how many elements are in a collection."),
      Details("""```
          |RECEIVED -> _pluralize(SINGULAR,PLURAL)
          |```
          |This is a convenient method for choosing different text depending on a Property. The RECEIVED
          |Context should usually be a List. If it contains a single element, _pluralize produces
          |SINGULAR; if it contains multiple *or* zero elements, _pluralize produces PLURAL.
          |
          |Note that this behaviour is pretty English-specific. We expect that other variations will
          |be needed for other languages in the long run.""".stripMargin)))
  {
    override def qlApply(inv:Invocation):QFut = {
      val context = inv.context
      val paramsOpt = inv.paramsOpt
      
      def chooseParam(params:Seq[QLParam]):QLExp = {
        val received = context.value
        if (received.isEmpty || received.size > 1)
          params(1).exp
        else
          params(0).exp
      }
      
      val result = for
      (
        params <- paramsOpt if params.length == 2;
        exp = chooseParam(params);
        parser <- context.parser
      )
        yield parser.processExp(exp, context.asCollection).map(_.value)
        
      result.getOrElse(QL.WarningFut("_pluralize requires exactly two parameters"))
    }
  }
  
  lazy val JoinMethod = new InternalMethod(JoinMethodOID,
    toProps(
      setName("_join"),
      Categories(TextTag),
      Summary("Combine a list of Text values together"),
      Details("""```
          |LIST -> _join(OPEN, SEP, CLOSE) -> QTEXT
          |```
          |_join takes the given LIST, and turns it into a single line. For example, if My List was "Cat", "Dog", "Horse",
          |then
          |```
          |My List -> _join
          |```
          |would come out as "CatDogHorse".
          |
          |Of course, that probably isn't what you want -- most of the time, you want some separators at the beginning,
          |middle and end. Those are the parameters; how many parameters you give define how they are used. If there is
          |only one, then it is SEP, the separator in between elements. So
          |```
          |My List -> _join(\"", \"")
          |```
          |would come out as "Cat, Dog, Horse" -- more reasonable.
          |
          |If there are two parameters, then they are OPEN and SEP. So for example, if I wanted to include dashes at the
          |beginning, that would be:
          |```
          |My List -> _join(\""-- \"", \"", \"")
          |```
          |which would come out as "-- Cat, Dog, Horse". And if I wanted parentheses around the entire list, I'd use all
          |three parameters -- OPEN, SEP and CLOSE -- as:
          |```
          |My List -> _join(\""(\"", \"", \"", \"")\"")
          |```
          |to get "(Cat, Dog, Horse)".
          |
          |Note that you can use _join with anything, not just Text -- if the received values aren't Text, then they will
          |be rendered into their default forms before getting combined. But at the end of _join, what you get back is
          |one big block of QText. You can't do any further processing on the elements after this.""".stripMargin)))
  {
    override def qlApply(inv:Invocation):QFut = {
      val context = inv.context
      val paramsOpt = inv.paramsOpt
      
      val (openPhrase, sepPhrase, closePhrase) = paramsOpt match {
        case Some(params) if (params.length == 1) => (None, Some(params(0)), None)
        case Some(params) if (params.length == 2) => (Some(params(0)), Some(params(1)), None)
        case Some(params) if (params.length > 2) => (Some(params(0)), Some(params(1)), Some(params(2)))
        case _ => (None, None, None)
      }
      def renderParam(paramOpt:Option[QLParam]):Future[Wikitext] = {
        paramOpt match {
          case Some(param) => {
            val collContext = context.asCollection
            for {
              paramVal <- context.parser.get.processExp(param.exp, collContext).map(_.value)
              renderedParam <- paramVal.firstOpt.map(elem => paramVal.pType.wikify(context)(elem)).getOrElse(Future.successful(Wikitext.empty))
            }
              yield renderedParam
          }
          case _ => Future.successful(Wikitext.empty)
        }
      }
  
      val elemT = context.value.pType
      for {
        renderedList <- Future.sequence(context.value.cv.map{elem => elemT.wikify(context)(elem)})   
        sep <- renderParam(sepPhrase)
        open <- renderParam(openPhrase)
        close <- renderParam(closePhrase)
        result =
          if (renderedList.isEmpty) {
            Wikitext.empty
          } else {
            open + (renderedList.head /: renderedList.tail) ((total, next) => total + sep + next) + close
          }
      }
        yield QL.WikitextValue(result)
    }
  }
  
  lazy val SubstringMethod = new InternalMethod(SubstringOID,
    toProps(
      setName("_substring"),
      SkillLevel(SkillLevelAdvanced),
      Signature(
        expected = Some((Seq(), "Anything -- each received item will be rendered as text, and then the substring taken from that.")),
        reqs = Seq(("start", IntType, "The zero-based index of the beginning of the substring")),
        opts = Seq(
            ("end", IntType, ExactlyOne(IntType(Int.MaxValue)), 
                "The zero-based index of the end of the substring -- if omitted, takes the rest of the string")),
        returns = (TextType, "The specified chunk out of the received text")
      ),
      Categories(TextTag),
      Summary("Extracts part of a Text or Large Text"),
      Details("""Given some text, this extracts the portion of it beginning at
        |character **start**, and running through **end** - 1.
        |
        |The parameters are zero-indexed; that is, the first character is 0, the second 1, etc. 
        |
        |This function is designed to deliberately ape the substring methods familiar from languages like
        |Java and JavaScript, and is mainly aimed at programmers. However, it is more forgiving than Java of indexes that
        |are out of bounds -- in particular, if END is beyond the end of the string, it will return the value
        |through the end of the string, and if START is beyond the end of the string, it will always return
        |the empty string.""".stripMargin)))
  {
    override def qlApply(inv:Invocation):QFut = {
      for {
        start <- inv.processAs("start", IntType)
        end <- inv.processAs("end", IntType)
        elemContext <- inv.contextElements
        qv = elemContext.value
        wikitext <- inv.fut(qv.wikify(elemContext))
        raw = wikitext.raw.toString()
        len = raw.length()
        finalStart = Math.min(start, len)
        finalEnd = Math.min(end, len)
        substr =
          if (end == Int.MaxValue)
            raw.substring(finalStart)
          else
            raw.substring(finalStart, finalEnd)
      }
        yield ExactlyOne(TextType(substr))
    }
  }
  
  // TODO: _matchCase() is really kind of deeply evil. It's *very* specialized, and we wouldn't have bothered with it
  // if it wasn't necessary to support the gender-matching in the LARP App.
  //
  // What we *should* do is rewrite this as a proper QL function. At the moment, it's flatly impossible in several ways,
  // but we should add enough machinery to make it possible. Prototype this in the LARP App. I suspect that performance
  // will be unacceptable in practice, but that will provide a nice driver for optimizing the QL pipeline. When we can
  // replace _matchCase as a pure-QL function built on top of more sensible primitives, we'll know that the pipeline is
  // getting to a nice level of functionality and performance.
  lazy val MatchCaseMethod = new InternalMethod(MatchCaseOID,
    toProps(
      setName("_matchCase"),
      SkillLevel(SkillLevelAdvanced),
      Categories(TextTag),
      Summary("Tweaks the case of the received Text to match that of the Function this is defined in."),
      Signature(
        expected = Some(Seq(TextType), "Some text, usually one word"),
        reqs = Seq.empty,
        opts = Seq(
          ("levels", IntType, ExactlyOne(IntType(1)), "The number of levels up that this will look for the match")
        ),
        returns = (TextType, "The received text, capitalized if this Function was capitalized")
      ),
      Details("""This is a very specialized function, designed for cases where you want to define a Text or Function
          |that results in some Text, but you want the *case* of that Text (upper or lower) to match the way
          |the Function was *invoked*.
          |
          |So for example, if you have a Function named Sie (one of the common choices for a gender-neutral
          |pronoun), which produces "he" if the received value is male or "she" if it's female, you would
          |use _matchCase so that, when I invoke it as \[[James -> sie\]] I get "he", but when I invoke it
          |as \[[Mary -> Sie\]], I get "She".
          |
          |Occasionally, you want to use this in a Function that is intended for use inside *other* Functions.
          |You can use the `levels` parameter to specify how far up to look. By default it is 1 (this Function);
          |if you set it to 2 it will be the Function that called this one, and so on.
          |
          |This will probably get moved to a text-manipulation Mixin at some time down the road.""".stripMargin)))
  {
    override def qlApply(inv:Invocation):QFut = {
      for {
        levels <- inv.processAs("levels", IntType)
        call <- inv.opt(findCall(inv, levels))
        text <- inv.contextAllAs(QL.ParsedTextType)
        adjusted = adjustCase(text, call)
      }
        yield ExactlyOne(QL.ParsedTextType(Wikitext(adjusted)))
    }
    
    // This is a slightly scary recursive algorithm. Basically, we are looking up the stack for
    // the call to the lexicalProp -- the Function that we're hunting. Making this more complex,
    // we may have to do this multiple times, if levels > 1 -- that's why we have two levels of
    // recursion. The outer recursion is the levels; the inner is hunting for the current level's
    // Function name.
    private def findCall(inv:Invocation, levels:Int):Option[QLCall] = {
      def findPropRec(outerContext:QLContext, remaining:Int):Option[QLCall] = {
        outerContext.parser.flatMap(_.lexicalProp) flatMap { thing =>
          def findRec(context:QLContext):Option[QLCall] = {
            val result = for {
              // fromTransformOpt indicates the actual Thing being called inside each QLCall:
              transformer <- context.fromTransformOpt
              if (transformer.id == thing.id)
              call <- context.withCallOpt
            }
              yield call
              
            result.flatMap { call =>
              if (remaining <= 1)
                Some(call)
              else
                // Found one, but we are supposed to keep looking further up the stack, so keep going:
                findPropRec(context, remaining - 1)
            }.orElse(context.parentOpt.flatMap(findRec(_)))
          }
          
          findRec(outerContext)
        }
      }
      
      findPropRec(inv.context, levels)
    }
    
    private def adjustCase(text:Wikitext, call:QLCall):String = {
      val charToMatch = call.name.name(0)
      val actualText = text.plaintext
      val charToAdjust = actualText(0)
      val adjusted = 
        if (charToMatch.isUpper)
          charToAdjust.toUpper
        else if (charToMatch.isLower)
          charToAdjust.toLower
        else
          // Odd...
          charToAdjust
      
      adjusted + actualText.substring(1)
    }
  }
  
  lazy val TextLengthMethod = new InternalMethod(TextLengthOID,
    toProps(
      setName("_textLength"),
      SkillLevel(SkillLevelAdvanced),
      Signature(
        expected = Some((Seq(AnyType), "A text value, or something that can be displayed as text")),
        reqs = Seq.empty,
        opts = Seq(
          ("text", AnyType, Core.QNone, "A text value, or something that can be displayed as text")
        ),
        returns = (IntType, "The length of the parameter, or of the received value if there is no parameter")
      ),
      Categories(TextTag),
      Summary("Produce the length of some received text.")))
  {
    override def qlApply(inv:Invocation):QFut = {
      for {
        elemContext <- inv.contextElements
        v = elemContext.value
        // TODO: Can we refactor out a common processParamOrContext function? Keep an eye on uses
        // of processAsOpt(), and see if it's worth lifting this out.
        textOpt <- inv.processAsOpt("text", QL.ParsedTextType, elemContext)
        wikitext <- inv.fut {
          textOpt match {
            case Some(wiki) => fut(wiki)
            case _ => v.wikify(elemContext)
          }
        }
        str = wikitext.strip.toString
      }
        yield ExactlyOne(IntType(str.length))
    }
  }
  
  override lazy val props = Seq(
    PluralizeMethod,
    JoinMethod,
    MatchCaseMethod,
    SubstringMethod,
    TextLengthMethod
  )
}
