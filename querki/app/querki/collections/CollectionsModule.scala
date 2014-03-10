package querki.collections

import models._
import querki.ecology._
import querki.ql.{QLParser, QLPhrase}
import querki.values._

object MOIDs extends EcotIds(6) {
  val FirstMethodOID = sysId(50)
  val IsEmptyOID = sysId(52)
  val IsNonEmptyOID = sysId(53)
  val FilterOID = sysId(55)
  val RestMethodOID = sysId(58)
  val SortMethodOID = sysId(61)
  val CountMethodOID = sysId(87)
  val ReverseMethodOID = sysId(88)
  val DescMethodOID = sysId(89)
  
  val PrevInListOID = moid(1)
  val NextInListOID = moid(2)
  val ForeachMethodOID = moid(3)
  val ContainsMethodOID = moid(4)
}

class CollectionsModule(e:Ecology) extends QuerkiEcot(e) with querki.core.MethodDefs with querki.logic.YesNoUtils {
  import MOIDs._

  lazy val Logic = interface[querki.logic.Logic]
  lazy val QL = interface[querki.ql.QL]
  
  /***********************************************
   * FUNCTIONS
   ***********************************************/
	
	lazy val FirstMethod = new InternalMethod(FirstMethodOID,
	    toProps(
	      setName("_first"),
	      Summary("""Grabs just the first thing from the received context."""),
	      Details("""    LIST -> _first -> OPTIONAL
	          |Often you have a List, and you just want the first item in the List. (Especially when you
	          |expect the list to only have one element in it.) Use _first to turn that List into an
	          |Optional instead.
	          |
	          |If LIST is empty, this produces None. If LIST has elements, this produces Optional(first element).""".stripMargin)))
	{
	  override def qlApply(inv:Invocation):QValue = {
	    val context = inv.context
	    
	    val sourceColl = context.value
	    val result = 
	      if (sourceColl.isEmpty)
	        Core.QNone
	      else
	        Optional(sourceColl.cv.head)
	    result
	  }
	}
	
	lazy val RestMethod = new InternalMethod(RestMethodOID,
	    toProps(
	      setName("_rest"),
	      Summary("""Produces everything but the first thing from the received context."""),
	      Details("""    LIST -> _rest -> LIST
	          |Often you have a List, and you want to slice off the first item (using _first). You then use _rest
	          |to handle everything else.
	          |
	          |_rest currently isn't useful very often. As the QL language gets more powerful, it will
	          |become a useful tool, although mainly for fairly advanced programmers.""".stripMargin)))
	{
	  override def qlApply(inv:Invocation):QValue = {
	    val context = inv.context
	    
	    val sourceColl = context.value
	    if (sourceColl.isEmpty)
	      // Cut processing at this point:
	      // TODO: can/should we preserve the source PType?
	      QL.EmptyListCut()
	    else
	      QList.makePropValue(sourceColl.cv.tail.toList, context.value.pType)
	  }
	}

	def isEmpty(inv:Invocation):Boolean = {
      val result = for (
	    thing <- inv.contextAllThings;
	    prop <- inv.definingContextAsProperty;
	    propAndVal <- inv.opt(thing.localProp(prop))
	  )
	    yield propAndVal.isEmpty
	      
	  if (result.get.isEmpty)
	    true
	  else
	    result.get.head	  
	}
	
	/**
	 * TBD: is this the correct definition of _isEmpty and _isNonEmpty? It feels slightly off to me, to have it specifically depend
	 * on the instance like this. But otherwise, we have problems because a property is essentially *always* non-Empty if it if defined,
	 * due to defaults.
	 * 
	 * Maybe the correct solution is a little more nuanced, that a property is considered "empty" if its value is the default?
	 */
	lazy val IsNonEmptyMethod = new InternalMethod(IsNonEmptyOID,
	    toProps(
	      setName("_isNonEmpty"),
	      Summary("Tests whether the provided value is non-empty"),
	      Details("""    THING -> PROP._isNonEmpty
	          |or
	          |    RECEIVED -> _isNonEmpty
	          |or
	          |    RECEIVED -> _isNonEmpty(PARAM)
	          |The first form produces true iff PROP is defined on THING, and this instance contains at least one element.
	          |
	          |The second form produces true iff RECEIVED contains at least one element.
	          |
	          |The third form runs the PARAM on the RECEIVED value, and produces true iff the result contains at least one element.
	          |
	          |This is usually used on a List, Set or Optional, but you *can* use it on an ExactlyOne value. (In which
	          |case it will always produce True.)""".stripMargin)))
	{
	  override def qlApply(inv:Invocation):QValue = {
	    if (inv.definingContext.isDefined)
	      boolean2YesNoQValue(!isEmpty(inv))
	    else 
	      for {
	        v <- inv.firstParamOrContextValue
	      }
	        yield boolean2YesNoQValue(!v.isEmpty)
	  }
	}
	
	lazy val IsEmptyMethod = new InternalMethod(IsEmptyOID,
	    toProps(
	      setName("_isEmpty"),
	      Summary("Tests whether the provided value is empty"),
	      Details("""    THING -> PROP._isEmpty
	          |or
	          |    RECEIVED -> _isEmpty
	          |or
	          |    RECEIVED -> _isEmpty(PARAM)
	          |The first form produces true iff PROP is not defined on THING, or the value is empty.
	          |
	          |The second form produces true iff RECEIVED is empty.
	          |
	          |The third form runs the PARAM on the RECEIVED value, and produces true iff the result is empty.
	          |
	          |This is usually used on a List, Set or Optional, but you *can* use it on an ExactlyOne value. (In which
	          |case it will always produce False.)""".stripMargin)))
	{
	  override def qlApply(inv:Invocation):QValue = {
	    if (inv.definingContext.isDefined)
	      boolean2YesNoQValue(isEmpty(inv))
	    else
	      for {
	        v <- inv.firstParamOrContextValue
	      }
  	        yield boolean2YesNoQValue(v.isEmpty)
	  }
	}
	
	lazy val FilterMethod = new InternalMethod(FilterOID,
	    toProps(
	      setName("_filter"),
	      Summary("Filter out non-matching elements of a collection"),
	      Details("""    RECEIVED -> _filter(FILTER)
	          |This function is how you take a List of things, and whittle them down to just the ones you want.
	          |
	          |The FILTER should take a Thing, and produce a YesNo that says whether to include this Thing.
	          |That gets applied to each element of RECEIVED; if FILTER returns Yes, then it is included, otherwise not.
	    	  |
	          |This is one of the most commonly-useful functions in Querki. It is how you usually say, "I only want *some*
	          |of the elements in this List or Set".""".stripMargin)))
	{
	  override def qlApply(inv:Invocation):QValue = {
	    val context = inv.context
	    val paramsOpt = inv.paramsOpt
	    
	    // TODO: this is currently convoluted and hard to understand -- we're dissecting the list using
	    // flatMapAsContext(); yielding an Option saying whether to keep each one; stitching it back together
	    // as a Context, and then just using the QValue. Bleah.
	    // TODO: this needs a major rewrite, to stop using so many QL internals!
	    def tryElem(parser:QLParser, phrase:QLPhrase)(elem:QLContext):Option[ElemValue] = {
	      val passesYesNo = parser.processPhrase(phrase.ops, elem).value
	      for (
	        bool <- passesYesNo.firstAs(YesNoType) if (bool);
	        theElem <- elem.value.firstOpt
	      ) yield theElem
	    }
	    
	    val result = for
	    (
	      params <- paramsOpt if params.length == 1;
	      phrase = params(0);
	      parser <- context.parser
	    )
	      yield context.flatMapAsContext(tryElem(parser, phrase), context.value.pType).value
	      
	    result.getOrElse(QL.WarningValue("_filter requires exactly one parameter"))
	  }
	}
	
	
	lazy val SortMethod = new InternalMethod(SortMethodOID,
	    toProps(
	      setName("_sort"),
	      Summary("Sort the received list"),
	      Details("""    LIST -> _sort -> SORTED
	          |or
	          |    LIST -> _sort(EXP) -> SORTED
	          |With no parameters (the first form), _sort sorts the elements of the received List alphabetically by their Display Names.
	          |This is what you want most of the time. However, note that many methods that return Lists are sorted to begin with,
	          |so you often don't even need to bother. (Sets of Links are always sorted by Display Name.)
	          |
	          |If a parameter is given (the second form), it is applied to each element in LIST, and the results are used to sort the
	          |elements. The sort order is whatever is natural for the returned elements -- usually alphabetical, but might be, for example,
	          |numeric if the results are numeric. It is essential that EXP return the same type for all elements, and it should return
	          |ExactlyOne value. (If it returns a List, only the first will be used. The behaviour is undefined if it returns a Set, or None.)
	          |
	          |At the moment, _sort is mainly designed for Links -- that is, pointers to Things -- since that is what 95% of use cases require. 
	          |We plan to make it more general, when folks come up with use cases that need it.
	          |
	          |Most of the time, you will want EXP to simply be the name of a Property. For example, this:
	          |
	          |    My Stuff._instance -> _sort(Title)
	          |
	          |Produces all of the Instances of the "My Stuff" Model, sorted based on the "Title" Property. But it's possible to get much fancier if you need to:
	          |EXP can be any QL Expression that receives a Link and produces a consistent Type.
	          |
	          |If you need to reverse the order of the sort, use the [[_desc._self]] method inside of it.
	          |
	          |If two or more elements being sorted have the same sort value, they will be sorted by Display Name.
	          |
	          |There is currently no way to define your own customized sort order. It'll probably happen someday, but will depend on user
	          |demand. It is likely that we will add the ability to sort by multiple keys (sort on Property A, then Property B if those are
	          |identical) in the not-too-distant future -- yell if this proves important for you.""".stripMargin)))
	{
	  override def qlApply(inv:Invocation):QValue = {
	    val context = inv.context
	    val paramsOpt = inv.paramsOpt
	    
	    implicit val s = context.state
	    implicit val rc = context.request
	
	    // TODO: this is awfully inefficient -- we're recomputing the processPhrase repeatedly during
	    // the sort process. We should probably instead map the parameter over the list, and then
	    // sort using the results:
	    def thingSortFunc(left:Thing, right:Thing):Boolean = {
	      val sortResult =
	        for (
	            params <- paramsOpt;
	            // This may return a wrapped Delegating Type. (Eg, DescendingType.)
	            leftCalc = context.parser.get.processPhrase(params(0).ops, context.next(ExactlyOne(LinkType(left)))).value;
	            // IMPORTANT: leftResult is the ElemValue, and its pType may *not* be the same as leftCalc! The ElemValue
	            // is the real element, which is likely to be of the underlying PType, without any _desc wrapper:
	            leftResult <- leftCalc.firstOpt orElse { Some(leftCalc.pType.default) };
	            rightCalc = context.parser.get.processPhrase(params(0).ops, context.next(ExactlyOne(LinkType(right)))).value;
	            rightResult <- rightCalc.firstOpt orElse { Some(rightCalc.pType.default) };
	            if (leftResult.pType.realType == rightResult.pType.realType);
	            // If the two values are equal, fall through to the default:
	            if (!leftCalc.pType.matches(leftResult, rightResult))
	          )
	          yield leftCalc.pType.comp(context)(leftResult, rightResult)
	
	      // Default to sorting by displayName if anything doesn't work correctly:
	      sortResult.getOrElse(left.displayName < right.displayName)
	    }
	    
	    val start = context.value.cv.toSeq
	    val pType = context.value.pType
	    pType match {
	      case LinkType => {
	        // TODO: we probably don't need to translate these to Things any more:
	        val asThings = start.map(elemV => context.state.anything(LinkType.get(elemV))).flatten
	        val sortedOIDs = asThings.sortWith(thingSortFunc).map(_.id)
	        // TODO: there is obviously a refactoring screaming to break free here, but it involves some fancy
	        // type math. How do we lift things so that we can do QList.from() an arbitrary PType? (Remember that
	        // it expects a PTypeBuilder, *and* requires that the input Iterable be of the expected RT.)
	        Core.listFrom(sortedOIDs, LinkType)
	      }
	      case _ => {
	        val sorted = start.sortWith(pType.comp(context))
	        QList.makePropValue(sorted, pType)
	      }
	    }
	  }
	}
	
	/**
	 * A pseudo-Type, which exists solely for the _desc method. This is a DelegatingType that is exactly like the one
	 * it wraps around, except that it has a reversed sort order.
	 */
	class DescendingType[VT](baseType: PType[VT]) extends DelegatingType[VT](baseType) {
	  override def doComp(context:QLContext)(left:VT, right:VT):Boolean = !realType.doComp(context)(left, right)
	}
	
	lazy val DescMethod = new InternalMethod(DescMethodOID,
	    toProps(
	      setName("_desc"),
	      Summary("Sort this list in descending order"),
	      Details("""    LIST -> _sort(_desc(EXP)) -> SORTED
	          |
	          |_desc returns the given EXP, tweaked so that the values in it have the reversed sort order from
	          |what they would normally have. It is usually used inside of _sort, to reverse the sort order, which
	          |is normally in ascending order.""".stripMargin)))
	{
	  override def qlApply(inv:Invocation):QValue = {
	    val context = inv.context
	    val paramsOpt = inv.paramsOpt
	    
	    paramsOpt match {
	      case Some(params) => {
	        val innerRes = context.parser.get.processPhrase(params(0).ops, context).value;
	        innerRes.cType.makePropValue(innerRes.cv, new DescendingType(innerRes.pType))
	      }
	      case None => WarningValue("_desc is meaningless without a parameter")
	    }
	  }
	}

	lazy val CountMethod = new InternalMethod(CountMethodOID,
	    toProps(
	      setName("_count"),
	      Summary("Produces the number of elements in the received Collection"),
	      Details("""    LIST -> _count -> NUMBER
	          |This is pretty much as simple as it sounds. It is most often used in the header of a _section, like this:
	          |    \[[My List -> _section(\""Items: (\[[_count\]])\"", _commas)\]]""".stripMargin)))
	{
	  override def qlApply(inv:Invocation):QValue = {
	    ExactlyOne(IntType(inv.context.value.cv.size))
	  }
	}
	
	lazy val ReverseMethod = new InternalMethod(ReverseMethodOID,
	    toProps(
	      setName("_reverse"),
	      Summary("Produces the same Collection it receives, as a List, in reverse order"),
	      Details("""    LIST -> _reverse -> REVERSED LIST
	          |
	          |This does exactly what it sounds like: it produces the same list, in reversed order.
	          |
	          |_reverse can technically be used on any Collection, but is only useful for Lists. However,
	          |it can be useful after sorting a Set:
	          |
	          |    SET -> _sort -> _reverse
	          |
	          |You can't _reverse a Set itself (Sets have their own intrinsic order), but _sort always
	          |produces a List.""".stripMargin)))
	{
	  override def qlApply(inv:Invocation):QValue = {
	    QList.makePropValue(inv.context.value.cv.toSeq.reverse.toList, inv.context.value.pType)
	  }
	}
  
  lazy val prevInListMethod = new InternalMethod(PrevInListOID,
      toProps(
        setName("_prevInList"),
        Summary("Fetch the previous value to this one from the given List"),
        Details("""    THING -> _prevInList(LIST) -> PREVIOUS THING 
        		|Given a THING, and a LIST that contains that THING, this returns the *previous* THING to that
        		|in the LIST. It returns None iff the THING is not in the LIST, or if it is the beginning of the LIST.""".stripMargin)))
  {
    override def qlApply(inv:Invocation):QValue = {
      val context = inv.context
      val paramsOpt = inv.paramsOpt
      
      paramsOpt match {
        case Some(params) if (params.length > 0) => {
          val thing = context.value.first
          val list = context.parser.get.processPhrase(params(0).ops, context).value
          val index = list.indexOf(thing)
          index match {
            case Some(i) => {
              if (i == 0)
                EmptyValue(thing.pType)
              else
                ExactlyOne(list.elemAt(i - 1))
            }
            case None => EmptyValue(thing.pType)
          }
        }
        case _ => WarningValue("_prevInList requires a List parameter")
      }
    }
  }

  lazy val nextInListMethod = new InternalMethod(NextInListOID,
      toProps(
        setName("_nextInList"),
        Summary("Fetch the next value to this one from the given List"),
        Details("""    THING -> _nextInList(LIST) -> NEXT THING 
        		|Given a THING, and a LIST that contains that THING, this returns the *next* THING to that
        		|in the LIST. It returns None iff the THING is not in the LIST, or if it is the end of the LIST.""".stripMargin)))
  {
    override def qlApply(inv:Invocation):QValue = {
      val context = inv.context
      val paramsOpt = inv.paramsOpt
      
      paramsOpt match {
        case Some(params) if (params.length > 0) => {
          val thing = context.value.first
          val list = context.parser.get.processPhrase(params(0).ops, context).value
          val index = list.indexOf(thing)
          index match {
            case Some(i) => {
              if (i == (list.size - 1))
                EmptyValue(thing.pType)
              else
                ExactlyOne(list.elemAt(i + 1))
            }
            case None => EmptyValue(thing.pType)
          }
        }
        case _ => WarningValue("_nextInList requires a List parameter")
      }
    }
  }
  
  lazy val foreachMethod = new InternalMethod(ForeachMethodOID,
      toProps(
        setName("_foreach"),
        Summary("Applies the parameter to each element in the received collection, and produces a collection of the results"),
        Details("""    COLL -> _foreach(param) -> RESULT 
        		|Otherwise known as "map" in many programming languages, this lets you take an expression or function
                |that operates on a single element, and apply it to each element in the received collection.""".stripMargin)))
  {
    override def qlApply(inv:Invocation):QValue = {
      for {
        elemContext <- inv.contextElements
        elemResult <- inv.processParam(0, elemContext)
      }
        yield elemResult
    }
  }
  
  lazy val containsMethod = new InternalMethod(ContainsMethodOID,
      toProps(
        setName("_contains"),
        Summary("Produces true if the received List contains the specified value"),
        Details("""    LIST -> _contains(VALUE) -> TRUE or FALSE
            |This checks each value in the received LIST; if any of them are _equal to the given VALUE,
            |this produces True.""".stripMargin)))
  {
    override def qlApply(inv:Invocation):QValue = {
	  val results:QValue = for {
	    compareTo <- inv.processParam(0)
	    elem <- inv.contextElements
	    elemV = elem.value
	  }
	    yield boolean2YesNoQValue(Logic.compareValues(elemV, compareTo)( (pt, elem1, elem2) => pt.matches(elem1, elem2) ))
	    
	  if (results.rawList(YesNoType).contains(true)) 
	    boolean2YesNoQValue(true)
	  else 
	    boolean2YesNoQValue(false)
    }
  }

  override lazy val props = Seq(
    FirstMethod,
    RestMethod,
    IsNonEmptyMethod,
    IsEmptyMethod,
    FilterMethod,
    SortMethod,
    DescMethod,
    CountMethod,
    ReverseMethod,
      
    prevInListMethod,
    nextInListMethod,
    foreachMethod,
    containsMethod
  )
  
}