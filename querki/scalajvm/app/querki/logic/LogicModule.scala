package querki.logic

import models.{OID, PType, ThingOps, ThingState}

import querki.ecology._
import querki.globals._
import querki.ql.{InvocationValue, QLPhrase}
import querki.util.{PublicException, QLog}
import querki.values._

object MOIDs extends EcotIds(9) {
  val FirstNonEmptyMethodOID = sysId(49)
  val NotOID = sysId(57)
  val IfMethodOID = sysId(64)
  val EqualsMethodOID = sysId(94)

  val TrueOID = moid(1)
  val FalseOID = moid(2)
  val OrMethodOID = moid(3)
  val AndMethodOID = moid(4)
  val LessThanMethodOID = moid(5)
  val GreaterThanMethodOID = moid(6)

  val AddMethodOID = moid(7)
  val AddNumericMethodOID = moid(8)
  val SubtractMethodOID = moid(9)
  val SubtractNumericMethodOID = moid(10)
  val TimesMethodOID = moid(11)
  val TimesNumericMethodOID = moid(12)
  val DivideMethodOID = moid(13)
  val DivideNumericMethodOID = moid(14)

  val MinMethodOID = moid(15)
  val MaxMethodOID = moid(16)
}

/**
 * This module should eventually contain everything pertaining to predicate logic.
 *
 * For now, most of the stuff that should be here is still in the core System Space,
 * but that should be fixed. This should absorb YesNoType, and all of the predicate-oriented
 * methods.
 */
class LogicModule(e: Ecology) extends QuerkiEcot(e) with YesNoUtils with querki.core.MethodDefs with Logic {
  import MOIDs._

  lazy val QL = interface[querki.ql.QL]

  val LogicTag = "Math and Logic"

  /**
   * ****************************************
   * FUNCTIONS
   * ****************************************
   */

  class FirstNonEmptyMethod
    extends InternalMethod(
      FirstNonEmptyMethodOID,
      toProps(
        setName("_firstNonEmpty"),
        Categories(LogicTag),
        Summary("""Produces the first parameter that is not empty."""),
        Details(
          """    RECEIVED -> _or(CLAUSE1, CLAUSE2, ...) -> RESULT
            |_or takes any number of parameters. It runs through each of them, applying the incoming context.
            |It produces the first one that returns a non-empty result, or None iff all of them come out empty.""".stripMargin
        )
      )
    ) {

    override def qlApply(inv: Invocation): QFut = {
      val context = inv.context
      val paramsOpt = inv.paramsOpt

      paramsOpt match {
        case Some(params) => {
          val resultFut = Future.sequence(params.map(param => context.parser.get.processExp(param.exp, context)))
          val result = resultFut.map(_.find(oneResult => !(oneResult.value.isEmpty)).map(_.value))

          // If we got nothing out, then produce an empty list of the incoming type
          // TBD: this really isn't the correct type to produce -- ideally, the type should
          // be the one that would be output by the various parameter phrases. How can we
          // suss that?
          result.map(_.getOrElse(EmptyValue(context.value.pType)))
        }
        case None => QL.WarningFut("_firstNonEmpty() is meaningless if you don't give it any parameters")
      }
    }
  }

  class NotMethod
    extends InternalMethod(
      NotOID,
      toProps(
        setName("_not"),
        Categories(LogicTag),
        Summary("Returns the reverse of the received value, or the parameter"),
        Details(
          """    TRUE/FALSE -> _not -> FALSE/TRUE
            |or
            |    RECEIVED -> _not(TRUE/FALSE) -> FALSE/TRUE
            |
            |_not takes the parameter if one is given, or the received value if not. It returns True iff that it False, and False if it is anything else""".stripMargin
        )
      )
    ) {

    override def qlApply(inv: Invocation): QFut = {
      val context = inv.context
      val paramsOpt = inv.paramsOpt

      val inVal = paramsOpt match {
        case Some(params) if (params.length == 1) => {
          context.parser.get.processExp(params(0).exp, context).map(_.value)
        }
        case _ => Future.successful(context.value)
      }
      inVal.map(v => !toBoolean(v))
    }
  }

  // TODO: this will become clearer and easier to use once we introduce block-syntax parameters.
  class IfMethod
    extends InternalMethod(
      IfMethodOID,
      toProps(
        setName("_if"),
        Categories(LogicTag),
        Summary("Choose what to produce, as directed"),
        Signature(
          expected = Some(Seq.empty, "Anything -- this will be passed into the predicate for testing"),
          reqs = Seq(
            ("predicate", YesNoType, "The question we're asking -- is this value true?"),
            ("iftrue", AnyType, "What to produce if the predicate is true")
          ),
          opts = Seq(
            ("iffalse", AnyType, Core.QNone, "What to produce if the predicate isn't true"),
            ("asList", YesNoType, false, "Whether to treat the context as a whole, or as individual elements")
          ),
          returns = (AnyType, "The result from `iftrue` if the `predicate` was true, `iffalse` (or nothing) otherwise")
        ),
        Details("""_if is one of the basic building blocks of programming. It applies the received value to the
                  |`predicate`. If the result is non-empty and is true, it applies the received value to `iftrue`. Otherwise,
                  |if there is an `iffalse` parameter, it applies the received value to that. (If there is no `iffalse`, it
                  |just produces the empty value.)
                  |
                  |For example, take this expression:
                  |```
                  |\[[My List -> _if(_isEmpty, \""Empty\"", \""Full\"")\]]
                  |```
                  |If `My List` is empty, this will print "Empty"; if not, it will print "Full".
                  |
                  |This is also an easy way to test TrueOrFalse flags. Say that our Space represents a library, and we
                  |want to print out whether a given book has the `Checked Out` flag set on it. We would say:
                  |```
                  |\[[My Book -> _if(Checked Out, \""Checked Out\"")\]]
                  |```
                  |So if `My Book -> Checked Out` is true, it will print "Checked Out". Since we didn't give an
                  |`iffalse` parameter, it doesn't print anything if the `Checked Out` flag isn't set.
                  |
                  |Note that, if `predicate` is empty, or the wrong type, that is treated as "false". If you're getting false
                  |answers and don't understand why, check what your `predicate` is actually producing.
                  |
                  |The syntax of _if is likely to evolve in the future, to something more like most programming languages. For now,
                  |though, note that `iftrue` and `iffalse` are *inside* the parentheses, rather than after them as most languages
                  |have it.
                  |
                  |Normally, `_if` processes the received context one element at a time: it checks the predicate against
                  |each element, and produces either the true or false path depending on the result of the predicate. But
                  |if you set the `asList` parameter to true, then it will run the predicate against the *entire* context
                  |at once, and do the true or false path on the *entire* context. This is appropriate if you are doing
                  |something with the whole list, like checking `_isEmpty`. 
                  |
                  |(*Note:* setting `asList` to true produces
                  |exactly the same result as putting an asterisk before the `_if`. An asterisk before a function
                  |means "do this to the entire List, not one at a time". The `asList` parameter is provided as an
                  |alternative in this common case, though, if you find it clearer.)""".stripMargin)
      )
    ) {

    override def qlApply(inv: Invocation): QFut = {
      for {
        asList <- inv.processAs("asList", YesNoType)
        ctx <-
          if (asList)
            inv.wrap(inv.context.asCollection)
          else
            inv.contextElements
        predicateOpt <- inv.processAsOpt("predicate", YesNoType, ctx)
        predicate = predicateOpt.getOrElse(false)
        result <-
          if (predicate)
            inv.process("iftrue", ctx)
          else
            inv.process("iffalse", ctx)
      } yield result
    }
  }

  def compareValues(
    firstIn: QValue,
    secondIn: QValue,
    method: String
  )(
    comparer: (PType[_], ElemValue, ElemValue) => Boolean
  ): QValue = {
    var first = firstIn
    var second = secondIn

    // TODO: conceptually, this is probably common code for any time where we care about multiple
    // values being the same Type. Not sure where it belongs, though.
    if (first.pType.realType != second.pType.realType) {
      first.coerceTo(second.pType.realType) match {
        case Some(coerced) => first = coerced
        case None => second.coerceTo(first.pType.realType) match {
            case Some(coerced) => second = coerced
            case None => throw new PublicException(
                "Logic.equals.typeMismatch",
                first.pType.displayName,
                second.pType.displayName,
                method
              )
          }
      }
    }

    val pt = first.pType
    if ((first.size == 1) && (second.size == 1)) {
      // The common and easy case:
      ExactlyOne(boolean2YesNo(comparer(pt, first.cv.head, second.cv.head)))
    } else {
      val allPairs = for {
        f <- first.cv
        s <- second.cv
      } yield comparer(pt, f, s)

      QList.makePropValue(allPairs.map(boolean2YesNo(_)), YesNoType)
    }
  }

  /**
   * The general pattern for binary operators.
   */
  def binaryComparer(
    inv: Invocation,
    method: String
  )(
    comparer: (PType[_], ElemValue, ElemValue) => Boolean
  ): QFut = {
    for {
      first <- inv.processParamNofM(0, 2)
      second <- inv.processParamNofM(1, 2)
    } yield compareValues(first, second, method)(comparer)
  }

  lazy val EqualsMethod = new InternalMethod(
    EqualsMethodOID,
    toProps(
      setName("_equals"),
      Categories(LogicTag),
      Summary("Do these parameters match?"),
      Details("""```
                |_equals(EXP1, EXP2) -> YES OR NO
                |```
                |_equals produces Yes iff the expressions in the two parameters match each other. The definition
                |of "match" is type-dependent, but by and large is similar to == in most programming languages.
                |
                |Alternate version:
                |```
                |VALUE -> _equals(EXP) -> YES OR NO
                |```
                |This receives a VALUE, and tells you whether it matches the given EXP.
                |
                |If you provide more than one value on either side, it will do *all* of the comparisons, and
                |return all of the results. So for example, if you say:
                |[[```
                |<2, 3, 4, 5, 6> -> _equals(4)
                |```]]
                |the result will be `false false true false false`.
                |""".stripMargin)
    )
  ) {

    override def qlApply(inv: Invocation): QFut = {
      binaryComparer(inv, "_equals")((pt, elem1, elem2) => pt.matches(elem1, elem2))
    }
  }

  lazy val LessThanMethod = new InternalMethod(
    LessThanMethodOID,
    toProps(
      setName("_lessThan"),
      Categories(LogicTag),
      Summary("Is the first parameter less than the second?"),
      Details("""```
                |_lessThan(EXP1, EXP2) -> YES OR NO
                |```
                |_lessThan produces Yes iff the value in the first expression is less than the second. The definition
                |of "less than" is type-dependent, but by and large is similar to < in most programming languages.
                |
                |Alternate version:
                |```
                |VALUE -> _lessThan(EXP) -> YES OR NO
                |```
                |This receives a VALUE, and tells you whether it is less than the given EXP.
                |
                |If you provide more than one value on either side, it will do *all* of the comparisons, and
                |return all of the results. So for example, if you say:
                |[[```
                |<2, 3, 4, 5, 6> -> _lessThan(4)
                |```]]
                |the result will be `true true false false false`.
                |""".stripMargin)
    )
  ) {

    override def qlApply(inv: Invocation): QFut = {
      binaryComparer(inv, "_lessThan")((pt, elem1, elem2) => pt.comp(inv.context)(elem1, elem2))
    }
  }

  lazy val GreaterThanMethod = new InternalMethod(
    GreaterThanMethodOID,
    toProps(
      setName("_greaterThan"),
      Categories(LogicTag),
      Summary("Is the first parameter greater than the second?"),
      Details(
        """```
          |_greaterThan(EXP1, EXP2) -> YES OR NO
          |```
          |_greaterThan produces Yes iff the value in the first expression is greater than the second. The definition
          |of "greater than" is type-dependent, but by and large is similar to > in most programming languages.
          |
          |Alternate version:
          |```
          |VALUE -> _greaterThan(EXP) -> YES OR NO
          |```
          |This receives a VALUE, and tells you whether it is greater than the given EXP.
          |
          |If you provide more than one value on either side, it will do *all* of the comparisons, and
          |return all of the results. So for example, if you say:
          |[[```
          |<2, 3, 4, 5, 6> -> _greaterThan(4)
          |```]]
          |the result will be `false false false true true`.
          |""".stripMargin
      )
    )
  ) {

    override def qlApply(inv: Invocation): QFut = {
      binaryComparer(inv, "_greaterThan") { (pt, elem1, elem2) =>
        // This one's slightly complex. pt.comp() returns false iff the second value is greater than *or* equal to
        // the first. So we need to also do an equality comparison.
        !pt.comp(inv.context)(elem1, elem2) && !pt.matches(elem1, elem2)
      }
    }
  }

  /**
   * From the received values, apply the "exp" parameter to each (if it exists), and apply the chooser function
   * to decide whether this value is "more" than the current accumulator.
   * @param inv The Invocation of the current function.
   * @param chooser A function that is given the PType (after applying exp if appropriate), the current
   *                accumulator, and the next value. It should return true if the next value is more than
   *                the accumulator, and false otherwise.
   * @return The "most" value from the results.
   */
  private def findMostOfList(inv: Invocation)(chooser: (PType[_], ElemValue, ElemValue) => Boolean): QFut = {
    // First, process all of the values, if they are to be processed, and flatten it down to pairs of
    // (received, processed), each of which is a single-element QValue:
    val processed: InvocationValue[(QValue, QValue)] =
      if (inv.numParams == 0)
        for {
          elemCtx <- inv.contextElements
          elemQV = elemCtx.value
        } yield (elemQV, elemQV)
      else
        for {
          elemCtx <- inv.contextElements
          qv <- inv.process("exp", elemCtx)
        } yield (elemCtx.value, qv)

    // Then find the "most" among them:
    processed.get.map { pairs: Iterable[(QValue, QValue)] =>
      if (pairs.isEmpty)
        Core.QNone
      else {
        val comparePt = pairs.head._2.pType
        val mostPair = (pairs.head /: pairs.tail) { (current, next) =>
          val (curReceived, curProcessed) = current
          val (nextReceived, nextProcessed) = next
          if (chooser(comparePt, curProcessed.first, nextProcessed.first))
            next
          else
            current
        }
        mostPair._1
      }
    }
  }

  lazy val MinMethod = new InternalMethod(
    MinMethodOID,
    toProps(
      setName("_min"),
      Categories(LogicTag),
      Summary("Produces the least of the received values"),
      Signature(
        expected = Some(Seq.empty, "A List of any sort"),
        reqs = Seq.empty,
        opts = Seq(
          ("exp", AnyType, Core.QNone, "An optional expression to apply to each received value")
        ),
        returns = (AnyType, "The lowest of the received values")
      ),
      Details(
        """In its simple form, with no parameters, `_min` takes a List of values, and
          |produces whatever is least from them:
          |[[```
          |<7, 28, 3, 5, 108> -> _min
          |```]]
          |produces 3.
          |
          |The more interesting and common usage, however, takes one parameter, which is an expression.
          |This applies the received values (usually Things) to that expression, and produces the one that
          |had the lowest result:
          |[[```
          |All Games -> _min(Price)
          |```]]
          |produces the Game with the lowest Price.
          |
          |You can get a similar result by using `_sort` and taking the first value of the results, but `_min`
          |is much, much faster, and should be used instead when possible.
        """.stripMargin
      )
    )
  ) {

    override def qlApply(inv: Invocation): QFut = {
      findMostOfList(inv) { (pt, current, next) => pt.comp(inv.context)(next, current) }
    }
  }

  lazy val MaxMethod = new InternalMethod(
    MaxMethodOID,
    toProps(
      setName("_max"),
      Categories(LogicTag),
      Summary("Produces the most of the received values"),
      Signature(
        expected = Some(Seq.empty, "A List of any sort"),
        reqs = Seq.empty,
        opts = Seq(
          ("exp", AnyType, Core.QNone, "An optional expression to apply to each received value")
        ),
        returns = (AnyType, "The highest of the received values")
      ),
      Details(
        """In its simple form, with no parameters, `_max` takes a List of values, and
          |produces whatever is most from them:
          |[[```
          |<7, 28, 3, 5, 108> -> _min
          |```]]
          |produces 108.
          |
          |The more interesting and common usage, however, takes one parameter, which is an expression.
          |This applies the received values (usually Things) to that expression, and produces the one that
          |had the highest result:
          |[[```
          |All Games -> _max(Price)
          |```]]
          |produces the Game with the highest Price.
          |
          |You can get a similar result by using `_sort` and taking the last value of the results, but `_max`
          |is much, much faster, and should be used instead when possible.
        """.stripMargin
      )
    )
  ) {

    override def qlApply(inv: Invocation): QFut = {
      findMostOfList(inv) { (pt, current, next) => pt.comp(inv.context)(current, next) }
    }
  }

  /**
   * Goes through a parameter list full of YesNo expressions, and returns the results.
   */
  private def computeBooleans(inv: Invocation): InvocationValue[Boolean] = {
    for {
      dummy <- inv.returnsType(YesNoType)
      paramNum <- inv.iter(0 until inv.numParams, None)
      paramVal <- inv.processParam(paramNum)
      paramVs = {
        if (paramVal.isEmpty)
          List.empty[Boolean]
        else
          paramVal.rawList(YesNoType)
      }
      paramResult <- inv.iter(paramVs)
    } yield paramResult
  }

  lazy val OrMethod = new InternalMethod(
    OrMethodOID,
    toProps(
      setName("_or"),
      Categories(LogicTag),
      Summary("Produces true iff any of the parameters are true"),
      Details("""```
                |_if(_or(VAL1, VAL2, VAL3...), RESULTS)
                |```
                |_or takes one or more parameters, and produces true if and only if at least one of the
                |elements in one of those parameters is true.""".stripMargin)
    )
  ) {

    override def qlApply(inv: Invocation): QFut = {
      val results = computeBooleans(inv)

      results.get.map(_.exists(v => v))
    }
  }

  lazy val AndMethod = new InternalMethod(
    AndMethodOID,
    toProps(
      setName("_and"),
      Categories(LogicTag),
      Summary("Produces true iff all of the parameters are true"),
      Details("""```
                |_if(_and(VAL1, VAL2, VAL3...), RESULTS)
                |```
                |_and takes one or more parameters, and produces true if and only if all of the elements in
                |all of those parameters are true.""".stripMargin)
    )
  ) {

    override def qlApply(inv: Invocation): QFut = {
      val results = computeBooleans(inv)

      results.get.map(r => !(r.exists(v => !v)))
    }
  }

  case class CompPair[N : Numeric](
    ns: List[N],
    ms: List[N],
    num: Numeric[N],
    pt: PType[N] with models.PTypeBuilder[N, N]
  ) {

    private def cross(f: (N, N) => N): List[N] = {
      for {
        n <- ns
        m <- ms
      } yield f(n, m)
    }

    private def combine(f: (N, N) => N): QValue = {
      val l = cross(f)
      if (l.size == 1)
        ExactlyOne(pt(l.head))
      else
        QList.makePropValue(l.map(pt(_)), pt)
    }

    def plus: QValue = combine(num.plus)
    def minus: QValue = combine(num.minus)
    def times: QValue = combine(num.times)

    // Garh. I really want to make this consistent, but the type math doesn't work:
    def divide: QValue = {
      val l = for {
        n <- ns
        m <- ms
      } yield (num.toDouble(n) / num.toDouble(m))

      val ft = Core.FloatType
      if (l.size == 1)
        ExactlyOne(ft(l.head))
      else
        QList.makePropValue(l.map(ft(_)), ft)
    }
  }

  def widenToMatch(
    qvn: QValue,
    qvm: QValue
  ): Option[CompPair[_]] = {
    val LongType = Core.LongType
    val FloatType = Core.FloatType
    if (qvn.isEmpty || qvm.isEmpty)
      None
    else {
      val comp = (qvn.pType, qvm.pType) match {
        case (IntType, IntType) =>
          CompPair(qvn.rawList(IntType), qvm.rawList(IntType), implicitly[Numeric[Int]], IntType)
        case (IntType, LongType) =>
          CompPair(qvn.rawList(IntType).map(_.toLong), qvm.rawList(LongType), implicitly[Numeric[Long]], LongType)
        case (IntType, FloatType) =>
          CompPair(qvn.rawList(IntType).map(_.toDouble), qvm.rawList(FloatType), implicitly[Numeric[Double]], FloatType)
        case (LongType, IntType) =>
          CompPair(qvn.rawList(LongType), qvm.rawList(IntType).map(_.toLong), implicitly[Numeric[Long]], LongType)
        case (LongType, LongType) =>
          CompPair(qvn.rawList(LongType), qvm.rawList(LongType), implicitly[Numeric[Long]], LongType)
        case (LongType, FloatType) => CompPair(
            qvn.rawList(LongType).map(_.toDouble),
            qvm.rawList(FloatType),
            implicitly[Numeric[Double]],
            FloatType
          )
        case (FloatType, IntType) =>
          CompPair(qvn.rawList(FloatType), qvm.rawList(IntType).map(_.toDouble), implicitly[Numeric[Double]], FloatType)
        case (FloatType, LongType) => CompPair(
            qvn.rawList(FloatType),
            qvm.rawList(LongType).map(_.toDouble),
            implicitly[Numeric[Double]],
            FloatType
          )
        case (FloatType, FloatType) =>
          CompPair(qvn.rawList(FloatType), qvm.rawList(FloatType), implicitly[Numeric[Double]], FloatType)
      }
      Some(comp)
    }
  }

  /**
   * The abstraction of "plus".
   *
   * TBD: this arguably belongs in a "math" Ecot that doesn't exist yet.
   *
   * When we get around to writing the Signature for this, note that OTHER is *not* necessarily the same type --
   * you can add a QDate and a Duration. So we're going to have to be fairly smart about how the signatures work.
   * This probably implies that we need to be able to declare AbstractFunctions with unbound type parameters, and
   * only bind them in the implementations. (Really, it implies that we need to be able to handle binding on
   * multiple types, to automatically catch errors at "compile" time.)
   */
  lazy val PlusMethod = new AbstractFunction(
    AddMethodOID,
    Received,
    toProps(
      setName("_plus"),
      Categories(LogicTag),
      Summary("Add two values together"),
      Details(
        """    VALUE -> _plus(OTHER) -> RESULT
          |This pretty much does what you would expect. However, note that it is only implemented for one
          |or two Types yet. If you have a case where you expect and need _plus to work, please drop us a note!""".stripMargin
      )
    )
  )

  lazy val plusNumericImpl =
    new FunctionImpl(AddNumericMethodOID, PlusMethod, Seq(IntType, Core.FloatType, Core.LongType)) {

      override def qlApply(inv: Invocation): QFut = {
        for {
          qvn <- inv.contextValue
          qvm <- inv.processParam(0)
          comp <- inv.opt(widenToMatch(qvn, qvm))
        } yield comp.plus
      }
    }

  lazy val MinusMethod = new AbstractFunction(
    SubtractMethodOID,
    Received,
    toProps(
      setName("_minus"),
      Categories(LogicTag),
      Summary("Subtract one value from another"),
      Details(
        """    VALUE -> _minus(OTHER) -> RESULT
          |This pretty much does what you would expect. However, note that it is only implemented for one
          |or two Types yet. If you have a case where you expect and need _minus to work, please drop us a note!""".stripMargin
      )
    )
  )

  lazy val minusNumericImpl =
    new FunctionImpl(SubtractNumericMethodOID, MinusMethod, Seq(IntType, Core.FloatType, Core.LongType)) {

      override def qlApply(inv: Invocation): QFut = {
        for {
          qvn <- inv.contextValue
          qvm <- inv.processParam(0)
          comp <- inv.opt(widenToMatch(qvn, qvm))
        } yield comp.minus
      }
    }

  lazy val TimesMethod = new AbstractFunction(
    TimesMethodOID,
    Received,
    toProps(
      setName("_times"),
      Categories(LogicTag),
      Summary("Multiple two values together"),
      Details(
        """    VALUE -> _times(OTHER) -> RESULT
          |This pretty much does what you would expect. However, note that it is only implemented for one
          |or two Types yet. If you have a case where you expect and need _times to work, please drop us a note!""".stripMargin
      )
    )
  )

  lazy val timesNumericImpl =
    new FunctionImpl(TimesNumericMethodOID, TimesMethod, Seq(IntType, Core.FloatType, Core.LongType)) {

      override def qlApply(inv: Invocation): QFut = {
        for {
          qvn <- inv.contextValue
          qvm <- inv.processParam(0)
          comp <- inv.opt(widenToMatch(qvn, qvm))
        } yield comp.times
      }
    }

  lazy val DivideMethod = new AbstractFunction(
    DivideMethodOID,
    Received,
    toProps(
      setName("_divideBy"),
      Categories(LogicTag),
      Summary("Divide one value by another"),
      Details(
        """    VALUE -> _divideBy(OTHER) -> RESULT
          |This pretty much does what you would expect. However, note that it is only implemented for one
          |or two Types yet. If you have a case where you expect and need _divideBy to work, please drop us a note!""".stripMargin
      )
    )
  )

  lazy val divideNumericImpl =
    new FunctionImpl(DivideNumericMethodOID, DivideMethod, Seq(IntType, Core.FloatType, Core.LongType)) {

      override def qlApply(inv: Invocation): QFut = {
        for {
          qvn <- inv.contextValue
          qvm <- inv.processParam(0)
          comp <- inv.opt(widenToMatch(qvn, qvm))
        } yield comp.divide
      }
    }

  override lazy val props = Seq(
    new FirstNonEmptyMethod,
    new NotMethod,
    new IfMethod,
    EqualsMethod,
    LessThanMethod,
    GreaterThanMethod,
    MinMethod,
    MaxMethod,
    OrMethod,
    AndMethod,
    PlusMethod,
    plusNumericImpl,
    MinusMethod,
    minusNumericImpl,
    TimesMethod,
    timesNumericImpl,
    DivideMethod,
    divideNumericImpl
  )

  /**
   * ****************************************
   * THINGS
   * ****************************************
   */

  class BooleanThingOps(v: BooleanValue) extends ThingOps(v) {
    override def qlApply(inv: Invocation): QFut = Future.successful(v.v)
  }

  class BooleanValue(
    tid: OID,
    elem: ElemValue,
    pm: PropMap
  ) extends ThingState(tid, systemOID, RootOID, pm) {
    val v = ExactlyOne(elem)
    override def thingOps(e: Ecology) = new BooleanThingOps(this)
  }

  lazy val trueVal = new BooleanValue(
    TrueOID,
    True,
    toProps(
      setName("True"),
      Categories(LogicTag),
      Summary("""The literal true value, for use in QL expressions.""")
    )
  )

  lazy val falseVal = new BooleanValue(
    FalseOID,
    False,
    toProps(
      setName("False"),
      Categories(LogicTag),
      Summary("""The literal false value, for use in QL expressions.""")
    )
  )

  override lazy val things = Seq(
    trueVal,
    falseVal
  )
}
