package querki.collections

import models._
import Thing._
import querki.ecology._
import querki.globals._
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
  val TakeOID = moid(5)
  val DropOID = moid(6)
  val ConcatOID = moid(7)
  val RandomOID = moid(8)
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
        Signature(
          expected = Some(Seq(AnyType), "A List of anything"),
          reqs = Seq.empty,
          opts = Seq.empty,
          returns = (AnyType, "The first element in the received list, or None if it was empty.")
        ),
	      Details("""Often you have a List, and you just want the first item in the List. (Especially when you
	          |expect the list to only have one element in it.) Use _first to turn that List into an
	          |Optional instead.""".stripMargin)))
	{
	  override def qlApply(inv:Invocation):QFut = {
	    val context = inv.context
	    
	    val sourceColl = context.value
	    val result = 
	      if (sourceColl.isEmpty)
	        Core.emptyOpt(sourceColl.pType)
	      else
	        Optional(sourceColl.cv.head)
          
	    Future.successful(result)
	  }
	}
	
	lazy val RestMethod = new InternalMethod(RestMethodOID,
	    toProps(
	      setName("_rest"),
	      Summary("""Produces everything but the first thing from the received context."""),
        Signature(
          expected = Some(Seq.empty, "A List of anything"),
          reqs = Seq.empty,
          opts = Seq.empty,
          returns = (AnyType, "The List of everything except the first element, or None if the received List had fewer than two elements in it.")
        ),
        Details("""Often you have a List, and you want to slice off the first item (using _first). You then use _rest
	          |to handle everything else.""".stripMargin)))
	{
	  override def qlApply(inv:Invocation):QFut = {
	    val context = inv.context
	    
	    val sourceColl = context.value
	    val result = if (sourceColl.isEmpty)
	      // Cut processing at this point:
	      // TODO: can/should we preserve the source PType?
	      QL.EmptyListCut()
	    else
	      QList.makePropValue(sourceColl.cv.tail.toList, context.value.pType)
        
      Future.successful(result)
	  }
	}
	
	lazy val TakeMethod = new InternalMethod(TakeOID,
	    toProps(
	      setName("_take"),
	      Summary("""Produces the first N values from the received context."""),
        Signature(
          expected = Some(Seq.empty, "A List of anything"),
          reqs = Seq(
            ("howMany", IntType, "The number of elements to take from the beginning of the List")
          ),
          opts = Seq.empty,
          returns = (AnyType, "The first *howMany* elements, if the received List had that many, or the full received List if not.")
        ),
	      Details("""Sometimes you want just the first few elements of a List. _take does that -- think of
	          |it as taking the beginning of the List, and leaving the rest behind.
	          |
	          |Note that `_first` is basically the same as `_take(1)`.""".stripMargin)))
	{
	  override def qlApply(inv:Invocation):QFut = {
	    for {
	      n <- inv.processAs("howMany", IntType)
	      qv <- inv.contextValue
	    }
	      yield QList.makePropValue(qv.cv.take(n), inv.context.value.pType)
	  }
	}
	
	lazy val DropMethod = new InternalMethod(DropOID,
	    toProps(
	      setName("_drop"),
	      Summary("""Produces all but the first N values from the received context."""),
        Signature(
          expected = Some(Seq.empty, "A List of anything"),
          reqs = Seq(
            ("howMany", IntType, "The number of elements to drop from the beginning of the List")
          ),
          opts = Seq.empty,
          returns = (AnyType, "The rest of the List after dropping the first *howMany* elements")
        ),
        Details("""_drop is the inverse of _take -- it drops the first *howMany*, and produces what's left,
	          |if anything.
	          |
	          |Note that _rest is roughly the same as _drop(1).""".stripMargin)))
	{
	  override def qlApply(inv:Invocation):QFut = {
	    for {
	      n <- inv.processAs("howMany", IntType)
	      qv <- inv.contextValue
	    }
	      yield QList.makePropValue(qv.cv.drop(n), inv.context.value.pType)
	  }
	}

	def isEmpty(inv:Invocation):Future[Boolean] = {
    val result = for {
	    thing <- inv.contextAllThings
	    prop <- inv.definingContextAsProperty
	    propAndVal <- inv.opt(thing.localProp(prop))
    }
	    yield propAndVal.isEmpty
      
    result.get.map(r => r.isEmpty || r.head)
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
        Signature(
          expected = Some(Seq.empty, "A Thing, or a List"),
          reqs = Seq.empty,
          opts = Seq(
            ("exp", AnyType, Core.QNone, "An expression to apply to the received value")
          ),
          returns = (YesNoType, "Yes if the list has anything in it; No if it is empty"),
          defining = Some(false, Seq(LinkType), "The Property to check whether its value is empty")
        ),
        Details("""There are several different versions of _isNonEmpty, depending on what you want to test:
            |```
            |Thing -> Property._isNonEmpty
            |```
            |Given a *Thing*, this checks whether it has *Property* and its value is not empty.
            |```
	          |List -> _isNonEmpty
            |```
            |Given a received List of any sort, this checks whether that List is not empty.
	          |```
	          |Anything -> _isNonEmpty(exp)
            |```
            |This is the most general form -- it applies *exp* to the received value, and checks whether
            |the result is empty. For example, if you have a Bookcase, and want to check whether any of the
            |Books on it were about Politics, it might look like:
            |```
            |My Bookcase -> _isNonEmpty(Books -> _filter(Topic -> _is(Politics)))
            |```
            |""".stripMargin)))
	{
	  override def qlApply(inv:Invocation):QFut = {
	    if (inv.definingContext.isDefined)
	      isEmpty(inv).map(v => boolean2YesNoQValue(!v))
	    else 
	      for {
          raw <- inv.rawParam("exp")
	        v <- raw match {
            case Some(_) => inv.process("exp")
            case _ => inv.contextValue
          }
	      }
	        yield boolean2YesNoQValue(!v.isEmpty)
	  }
	}
	
	lazy val IsEmptyMethod = new InternalMethod(IsEmptyOID,
	    toProps(
	      setName("_isEmpty"),
	      Summary("Tests whether the provided value is empty"),
        Signature(
          expected = Some(Seq.empty, "A Thing, or a List"),
          reqs = Seq.empty,
          opts = Seq(
            ("exp", AnyType, Core.QNone, "An expression to apply to the received value")
          ),
          returns = (YesNoType, "No if the list has anything in it; Yes if it is empty"),
          defining = Some(false, Seq(LinkType), "The Property to check whether its value is empty")
        ),
        Details("""There are several different versions of _isEmpty, depending on what you want to test:
            |```
            |Thing -> Property._isEmpty
            |```
            |Given a *Thing*, this checks whether it has *Property* and its value is empty.
            |```
            |List -> _isEmpty
            |```
            |Given a received List of any sort, this checks whether that List is empty.
            |```
            |Anything -> _isEmpty(exp)
            |```
            |This is the most general form -- it applies *exp* to the received value, and checks whether
            |the result is empty. For example, if you have a Bookcase, and want to check whether it is
            |empty of Books, you might say:
            |```
            |My Bookcase -> _isEmpty(Books)
            |```
            |""".stripMargin)))
	{
	  override def qlApply(inv:Invocation):QFut = {
	    if (inv.definingContext.isDefined)
	      isEmpty(inv).map(v => boolean2YesNoQValue(v))
	    else
	      for {
          raw <- inv.rawParam("exp")
          v <- raw match {
            case Some(_) => inv.process("exp")
            case _ => inv.contextValue
          }
	      }
  	        yield boolean2YesNoQValue(v.isEmpty)
	  }
	}
	
	lazy val FilterMethod = new InternalMethod(FilterOID,
	    toProps(
	      setName("_filter"),
	      Summary("Filter out non-matching elements of a collection"),
        Signature(
          expected = Some(Seq.empty, "A List of any sort"),
          reqs = Seq(("exp", YesNoType, "An expression to apply to the received value, which should produce True or False")),
          opts = Seq.empty,
          returns = (AnyType, "The elements of the List for which *exp* produced True")
        ),
	      Details("""This function is how you take a List of things, and whittle them down to just the ones you want.
	          |
	          |*exp* should take one element from the received List, and produce a YesNo that says whether to include this Thing.
	          |That gets applied to each element of the List; if *exp* returns Yes, then it is included, otherwise not.
            |
            |IMPORTANT: if *exp*'s result is empty for one of the elements, it is considered to be No -- the element
            |will be left out of the results.
	    	    |
	          |This is one of the most commonly-useful functions in Querki. It is how you usually say, "I only want *some*
	          |of the elements in this List or Set".""".stripMargin)))
	{
	  override def qlApply(inv:Invocation):QFut = {
      for {
        dummy <- inv.preferCollection(QList)
        elemContext <- inv.contextElements
        passes <- inv.processAs("exp", YesNoType, elemContext)
        if (passes)
      }
        yield elemContext.value
	  }
	}
	
	lazy val SortMethod = new InternalMethod(SortMethodOID,
	    toProps(
	      setName("_sort"),
	      Summary("Sort the received list"),
        Signature(
          expected = Some(Seq.empty, "A List of any sort"),
          reqs = Seq.empty,
          opts = Seq(("exp", AnyType, Core.QNone, "One or more expressions to apply to the received values, saying how to sort them")),
          returns = (AnyType, "The same List, sorted as requested")
        ),        
	      Details("""With no parameters, _sort sorts the elements of the received List "naturally" -- alphabetically by their Display Names
            |if they are Things, in numeric order if they are Numbers, and so on.
	          |This is what you want most of the time. However, note that many methods that return Lists are sorted to begin with,
	          |so you often don't even need to bother.
	          |
	          |If parameters are given, they are applied to each element in LIST, and the results are used to sort the
	          |elements. The sort order is whatever is natural for the returned elements -- usually alphabetical, but might be, for example,
	          |numeric if the results are numeric. It is essential that EXP return the same type for all elements, and it should return
	          |ExactlyOne value. (If it returns a List, only the first will be used. The behaviour is undefined if it returns a Set, or None.)
	          |
	          |If you need to sort on multiple fields, list them as additional parameters. For example, if you have
	          |```
	          |LIST -> _sort(A, B, C) -> SORTED
	          |```
	          |This will try to sort the list on A. When it finds multiple Things with the same value for A, it will sort them on B instead, then
	          |C, and so on.
	          |
	          |_sort is focused on Links, and as mentioned above, will default to sorting those by Display Name. You can also sort lists of Numbers,
	          |and many Types just work correctly, but not all. If you need to sort something, and can't make it work, please speak up.
	          |
	          |Most of the time, you will want *exp* to simply be the name of a Property. For example, this:
	          |```
	          |My Stuff._instance -> _sort(Title)
	          |```
	          |Produces all of the Instances of the "My Stuff" Model, sorted based on the "Title" Property. But it's possible to get much fancier if you need to:
	          |*exp* can be any QL Expression that receives a Link and produces a consistent Type.
	          |
	          |If you need to reverse the order of the sort, use the [[_desc._self]] method inside of it.
	          |
	          |If two or more elements being sorted have the same sort value, they will be sorted by Display Name.""".stripMargin)))
	{
	  // Note that we intentionally need to keep the type to compare on separate from the actual ElemValue.
	  // That is because the compType might be, eg, a _desc() wrapper around the actual PType.
	  sealed trait SortTerm
	  case object EmptySortTerm extends SortTerm
	  case class RealSortTerm(elem:ElemValue, compType:PType[_]) extends SortTerm
	  case class SortTerms(t:Thing, terms:Seq[SortTerm])(implicit s:SpaceState, rc:RequestContext) {
      // TODO: HORRIBLE HACK!
      // This is a compromise, and should be a temporary one. The issue is that we don't want to calculate the
      // displayName unless we must, because Computed Name can take a long time. But Seq.sortWith() is synchronous:
      // it doesn't allow the sort function to be Future[Boolean]. So for the moment, we're allowing ourselves to
      // compute it lazily, blocking.
      // The correct solution here is probably to write a Future-friendly version of sortWith(), where each computation
      // returns Future[Boolean] and it composes the result. That's a project, though, so I'm not dealing with it yet.
      lazy val displayName = awaitHack(t.unsafeNameOrComputed)
    }
	  
	  override def qlApply(inv:Invocation):QFut = {
	    val context = inv.context
	    val paramsOpt = inv.paramsOpt
	    
	    implicit val s = context.state
	    implicit val rc = context.request
	
	    /**
	     * Compare two SortTerms.
	     */
	    def thingSortFunc(left:SortTerms, right:SortTerms):Boolean = {
	      val matchedTerms = left.terms zip right.terms
	      // Iterate over the paired elements, which correspond to the parameters to _sort. Use the first
	      // result where they don't match:
	      val resultOpt = (Option.empty[Boolean] /: matchedTerms) { (current, terms) =>
	        current match {
	          // We've already found a valid pair to sort on, so skip the rest:
	          case Some(b) => current
	          
	          // Still trying, so let's see how this pair does:
	          case None => {
	            val (left, right) = terms
	            left match {
	              case EmptySortTerm => {
	                right match {
	                  // Both empty, so move along to the next term...
	                  case EmptySortTerm => None
	                  // Left is empty, right isn't -- left is "less than" right
	                  case _ => Some(true)
	                }
	              }
	              case RealSortTerm(leftElem, compType) => {
	                right match {
	                  // Left is real, right is empty -- right is "less than" left
	                  case EmptySortTerm => Some(false)
	                  case RealSortTerm(rightElem, _) => {
	                    // Okay, they're both non-empty:
	                    // NOTE: why do we have to check that the types match? Because Querki is weakly typed, so left and right
	                    // could wind up producing different types from the same test expression.
	                    // TBD: in the long run, we might wind up with "subtype" relationships, in which case this test
	                    // will need to become more sophisticated:
    			            if (leftElem.pType.realType == rightElem.pType.realType && !compType.matches(leftElem, rightElem))
    			              // They're the same type, and don't match, so let's compare:
    			              Some(compType.comp(context)(leftElem, rightElem))
    			            else
    			              // Either they're not matching types, or they're identical, so move along:
    			              None
	                  }
	                }
	              }
	            }
	          }
	        }
	      }
	      
	      // When all else fails, compare display names:
	      resultOpt.getOrElse(left.displayName < right.displayName)
	    }
	    
	    /**
	     * For a given Thing, compute its sort terms based on the parameters. The last element of the tuple is the display name,
	     * which is the final fallback for sorting.
	     */
	    def computeSortTerms(t:Thing):Future[SortTerms] = {
	      paramsOpt match {
	        case Some(params) => {
	          val termFuts:Seq[Future[SortTerm]] = for {
              // For each param...
	            param <- params
	          }
              yield for {
                // Do some calculations and get a Future...
                // This may return a wrapped Delegating Type. (Eg, DescendingType.)
                tCalc <- context.parser.get.processPhrase(param.phrase.ops, context.next(ExactlyOne(LinkType(t)))).map(_.value)
                tRawResultOpt = tCalc.firstOpt
                // Note that tResultOpt will be None iff the processing came up empty, or as UnknownOID. (The latter
                // is very common iff the sort expression including a Property not defined on the received Bundle.)
                tResultOpt = {
                  tRawResultOpt match {
                    case None => None
                    case Some(result) => {
                      result.getOpt(LinkType) match {
                        // This is the heart of this big clause: translate UnknownOID to EmptySortTerm
                        case Some(oid) => if (oid == UnknownOID) None else Some(result)
                        case None => Some(result)
                      }
                    }
                  }
                }                
              }
  	            // IMPORTANT: tResult is the ElemValue, and its pType may *not* be the same as tCalc! The ElemValue
  	            // is the real element, which is likely to be of the underlying PType, without any _desc wrapper
  	            yield tResultOpt.map(tResult => RealSortTerm(tResult, tCalc.pType)).getOrElse(EmptySortTerm)
	          
            // Finally, compose the Futures together:
            for {
              terms <- Future.sequence(termFuts)
            }
  	          yield SortTerms(t, terms)
	        }
	        // The simple case: there are no sort parameters, so we're just sorting on displayName.
	        case None => Future.successful(SortTerms(t, Seq.empty))
	      }
	    }
	    
	    val start = context.value.cv.toSeq
	    val pType = context.value.pType
	    pType match {
	      case LinkType => {
	        val asThings = start.map(elemV => context.state.anything(LinkType.get(elemV))).flatten
	        Future.sequence(asThings.map(computeSortTerms)).map { terms =>
            val sortedOIDs = terms.sortWith(thingSortFunc).map(_.t.id)
            Core.listFrom(sortedOIDs, LinkType)            
          }
	      }
	      case _ => {
	        val sorted = start.sortWith(pType.comp(context))
	        Future.successful(QList.makePropValue(sorted, pType))
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
        Signature(
          expected = Some(Seq.empty, "A List of any sort"),
          reqs = Seq(("exp", AnyType, "An expression, that you would use in _sort, which you want to reverse the order for")),
          opts = Seq.empty,
          returns = (AnyType, "The sort term, reversed")
        ), 
        Details("""_desc returns the given EXP, tweaked so that the values in it have the reversed sort order from
	          |what they would normally have. It is usually used inside of _sort, to reverse the sort order, which
	          |is normally in ascending order.
            |
            |For example, say that your Space was for Groceries, and you wanted to sort them by Price, with the most
            |expensive on top. That would look something like:
            |```
            |\[[Grocery Item._instances -> _sort(_desc(Price))\]]
            |```
            |That is, "Sort the instances of Grocery Item, in descending order of Price". """.stripMargin)))
	{
	  override def qlApply(inv:Invocation):QFut = {
      for {
        v <- inv.process("exp")
      }
        yield v.cType.makePropValue(v.cv, new DescendingType(v.pType))
	  }
	}

	lazy val CountMethod = new InternalMethod(CountMethodOID,
	    toProps(
	      setName("_count"),
	      Summary("Produces the number of elements in the received Collection"),
        Signature(
          expected = Some(Seq.empty, "A List of any sort"),
          reqs = Seq.empty,
          opts = Seq.empty,
          returns = (IntType, "How many elements are in that List")
        ),        
	      Details("""This is pretty much as simple as it sounds. It is most often used in the header of a _section, like this:
	          |    \[[My List -> _section(\""Items: (\[[_count\]])\"", _commas)\]]""".stripMargin)))
	{
	  override def qlApply(inv:Invocation):QFut = {
	    Future.successful(ExactlyOne(IntType(inv.context.value.cv.size)))
	  }
	}
	
	lazy val ReverseMethod = new InternalMethod(ReverseMethodOID,
	    toProps(
	      setName("_reverse"),
	      Summary("Produces the same Collection it receives, as a List, in reverse order"),
        Signature(
          expected = Some(Seq.empty, "A List of any sort"),
          reqs = Seq.empty,
          opts = Seq.empty,
          returns = (AnyType, "The same List, in reverse order")
        ),        
	      Details("""This does exactly what it sounds like: it produces the same list, in reversed order.
	          |
	          |_reverse can technically be used on any Collection, but is only useful for Lists. However,
	          |it can be useful after sorting a Set:
	          |```
	          |SET -> _sort -> _reverse
	          |```
	          |You can't _reverse a Set itself (Sets have their own intrinsic order), but _sort always
	          |produces a List.""".stripMargin)))
	{
	  override def qlApply(inv:Invocation):QFut = {
	    Future.successful(QList.makePropValue(inv.context.value.cv.toSeq.reverse.toList, inv.context.value.pType))
	  }
	}
  
  lazy val prevInListMethod = new InternalMethod(PrevInListOID,
      toProps(
        setName("_prevInList"),
        SkillLevel(SkillLevelAdvanced),
        Summary("Fetch the previous value to this one from the given List"),
        Signature(
          expected = Some(Seq.empty, "Something that you expect to find in the List"),
          reqs = Seq(("list", AnyType, "An expression that produces a List")),
          opts = Seq.empty,
          returns = (AnyType, "The previous element in *list*. None if the received value was at the beginning, or isn't contained in the *list*")
        ),        
        Details("""Given a received value, and a *list* that contains that value, this returns the *previous* element to that
        		|in the *list*.
            |
            |For example, say that your Space has a collection of Fruit, including Apple, Banana, Blackberry, Kiwi and Pear, and you
            |want to be able to navigate between them. In the Default View for Fruit, you could create a button that goes to the
            |previous one like this:
            |```
            |\[[_prevInList(Fruit._instances -> _sort) -> _linkButton(\""Previous Fruit\"")\]]
            |```""".stripMargin)))
  {
    override def qlApply(inv:Invocation):QFut = {
      for {
        v <- inv.process("list")
      }
      yield {
        val elem = inv.context.value.first
        val index = v.indexOf(elem)
        index match {
          case Some(i) => {
            if (i == 0)
              EmptyValue(elem.pType)
            else
              ExactlyOne(v.elemAt(i - 1))
          }
          case None => EmptyValue(elem.pType)
        }
      }
    }
  }
  
  lazy val nextInListMethod = new InternalMethod(NextInListOID,
      toProps(
        setName("_nextInList"),
        SkillLevel(SkillLevelAdvanced),
        Summary("Fetch the next value to this one from the given List"),
        Signature(
          expected = Some(Seq.empty, "Something that you expect to find in the List"),
          reqs = Seq(("list", AnyType, "An expression that produces a List")),
          opts = Seq.empty,
          returns = (AnyType, "The next element in *list*. None if the received value was at the end, or isn't contained in the *list*")
        ),        
        Details("""Given a received value, and a *list* that contains that value, this returns the *next* element to that
            |in the *list*.
            |
            |For example, say that your Space has a collection of Fruit, including Apple, Banana, Blackberry, Kiwi and Pear, and you
            |want to be able to navigate between them. In the Default View for Fruit, you could create a button that goes to the
            |next one like this:
            |```
            |\[[_nextInList(Fruit._instances -> _sort) -> _linkButton(\""Next Fruit\"")\]]
            |```""".stripMargin)))
  {
    override def qlApply(inv:Invocation):QFut = {
      for {
        v <- inv.process("list")
      }
      yield {
        val elem = inv.context.value.first
        val index = v.indexOf(elem)
        index match {
          case Some(i) => {
            if (i == (v.size - 1))
              EmptyValue(elem.pType)
            else
              ExactlyOne(v.elemAt(i + 1))
          }
          case None => EmptyValue(elem.pType)
        }
      }
    }
  }
  
  lazy val foreachMethod = new InternalMethod(ForeachMethodOID,
      toProps(
        setName("_foreach"),
        Summary("Applies the parameter to each element in the received collection, and produces a collection of the results"),
        Signature(
          expected = Some(Seq.empty, "A List or Set of any sort"),
          reqs = Seq(("exp", AnyType, "An expression to run on each element of the collection")),
          opts = Seq.empty,
          returns = (AnyType, "The results of running *exp* on each element")
        ), 
        Details("""Otherwise known as "map" in many programming languages, this lets you take an expression or function
                |that operates on a single element, and apply it to each element in the received collection.""".stripMargin)))
  {
    override def qlApply(inv:Invocation):QFut = {
      for {
        elemContext <- inv.contextElements
        elemResult <- inv.process("exp", elemContext)
      }
        yield elemResult
    }
  }
  
  lazy val containsMethod = new InternalMethod(ContainsMethodOID,
    toProps(
      setName("_contains"),
      Summary("Produces true if the received List contains the specified value"),
      Signature(
        expected = Some(Seq.empty, "A List (or Set) of any sort"),
        reqs = Seq(("v", AnyType, "A single value that might be in the List. This should be Exactly One; if not, the first element will be used.")),
        opts = Seq.empty,
        returns = (YesNoType, "True if *v* is found in the List; False otherwise")
      )))
  {
    override def qlApply(inv:Invocation):QFut = {
      for {
        compareTo <- inv.process("v")
        elemOpt = compareTo.firstOpt
        result = elemOpt match {
          case Some(elem) => inv.context.value.contains(elem)
          case _ => false
        }
      }
        yield boolean2YesNoQValue(result)
    }
  }

  lazy val ConcatMethod = new InternalMethod(ConcatOID,
    toProps(
      setName("_concat"),
      Summary("Concatenates the Lists given as parameters"),
      Signature(
        expected = None,
        reqs = Seq(("lists", AnyType, "One or more lists")),
        opts = Seq.empty,
        returns = (AnyType, "All of the given *lists*, concatenated together")
      ), 
      Details("""Occasionally, you want to take several separate lists, and treat them as a single list. This allows
          |you to do something like
          |```
          |\[[My Thing -> _concat(Primary Sources, Secondary Sources) -> _bulleted\]]
          |```
          |Note that the received context will be passed to each of the expressions in *lists*, as usual, but
          |otherwise isn't relevant. You can write expressions that completely ignore the received context.""".stripMargin)))
  {
    override def qlApply(inv:Invocation):QFut = {
  	  for {
        firstParam <- inv.processParam(0)
        targetType = firstParam.pType
  	    n <- inv.iter(0 to (inv.numParams-1))
  	    paramVals <- inv.processParam(n)
  	    typeCheck <- inv.test(paramVals.pType == targetType, "Collections.concat.mismatchedTypes", { Seq(targetType.displayName, paramVals.pType.displayName) })
  	  }
  	    yield paramVals
  	}
  }
  
  lazy val RandomMethod = new InternalMethod(RandomOID,
    toProps(
      setName("_random"),
      Summary("Select an item at random from a list"),
      Signature(
        expected = Some(Seq.empty, "A List"),
        reqs = Seq.empty,
        opts = Seq.empty,
        returns = (AnyType, "A random element from the List")
      ), 
      Details("""Most of the time, you want your Querki pages to be nice and predictable. But occasionally,
          |you might want some randomness. For example, if you have a Cookbook Space, you might want a
          |page that gives you a recipe at random, to give you ideas for dinner. The _random function
          |does that, allowing you to do something like:
          |```
          |\[[Recipes._instances -> _random\]]
          |```""".stripMargin)))
  {
    override def qlApply(inv:Invocation):QFut = {
      import scala.math._
      for {
        v <- inv.contextValue
        len = v.cv.size
        rnd = floor(random * len).toInt
        item = v.cv.drop(rnd).head
      }
        yield ExactlyOne(item)
    }
  }
	
  override lazy val props = Seq(
    FirstMethod,
    RestMethod,
    TakeMethod,
    DropMethod,
    IsNonEmptyMethod,
    IsEmptyMethod,
    FilterMethod,
    SortMethod,
    DescMethod,
    CountMethod,
    ReverseMethod,
    ConcatMethod,
    RandomMethod,
      
    prevInListMethod,
    nextInListMethod,
    foreachMethod,
    containsMethod
  )
  
}