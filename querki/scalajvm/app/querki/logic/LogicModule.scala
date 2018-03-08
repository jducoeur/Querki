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
}

/**
 * This module should eventually contain everything pertaining to predicate logic.
 * 
 * For now, most of the stuff that should be here is still in the core System Space,
 * but that should be fixed. This should absorb YesNoType, and all of the predicate-oriented
 * methods.
 */
class LogicModule(e:Ecology) extends QuerkiEcot(e) with YesNoUtils with querki.core.MethodDefs with Logic {
  import MOIDs._
  
  lazy val QL = interface[querki.ql.QL]
  
  val LogicTag = "Math and Logic"

  /******************************************
   * FUNCTIONS
   ******************************************/
  
  class FirstNonEmptyMethod extends InternalMethod(FirstNonEmptyMethodOID,
    toProps(
      setName("_firstNonEmpty"),
      Categories(LogicTag),
      Summary("""Produces the first parameter that is not empty."""),
      Details("""    RECEIVED -> _or(CLAUSE1, CLAUSE2, ...) -> RESULT
          |_or takes any number of parameters. It runs through each of them, applying the incoming context.
          |It produces the first one that returns a non-empty result, or None iff all of them come out empty.""".stripMargin)))
  {
    override def qlApply(inv:Invocation):QFut = {
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
  
  class NotMethod extends InternalMethod(NotOID,
    toProps(
      setName("_not"),
      Categories(LogicTag),
      Summary("Returns the reverse of the received value, or the parameter"),
      Details("""    TRUE/FALSE -> _not -> FALSE/TRUE
          |or
          |    RECEIVED -> _not(TRUE/FALSE) -> FALSE/TRUE
          |
          |_not takes the parameter if one is given, or the received value if not. It returns True iff that it False, and False if it is anything else""".stripMargin)))
  {
    override def qlApply(inv:Invocation):QFut = {
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
  class IfMethod extends InternalMethod(IfMethodOID,
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
          ("iffalse", AnyType, Core.QNone, "What to produce if the predicate isn't true")
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
          |have it.""".stripMargin)))
  {
    override def qlApply(inv:Invocation):QFut = {
      for {
        predicateOpt <- inv.processAsOpt("predicate", YesNoType)
        predicate = predicateOpt.getOrElse(false)
        result <-
          if (predicate)
            inv.process("iftrue")
          else
            inv.process("iffalse")
      }
        yield result
    }
  }
  
  def compareValues(firstIn:QValue, secondIn:QValue, method:String)(comparer:(PType[_], ElemValue, ElemValue) => Boolean):Boolean = {
    var first = firstIn
    var second = secondIn
    
    // TODO: conceptually, this is probably common code for any time where we care about multiple
    // values being the same Type. Not sure where it belongs, though.
    if (first.pType.realType != second.pType.realType) {
      first.coerceTo(second.pType.realType) match {
        case Some(coerced) => first = coerced
        case None => second.coerceTo(first.pType.realType) match {
          case Some(coerced) => second = coerced
          case None => throw new PublicException("Logic.equals.typeMismatch", first.pType.displayName, second.pType.displayName, method)
        }
      }
    }
        
    if (first.size == second.size) {
      val pt = first.pType
      val pairs = first.cv.zip(second.cv)
      pairs.forall(pair => comparer(pt, pair._1, pair._2))
    } else {
      false
    }    
  }

  /**
   * The general pattern for binary operators.
   */
  def binaryComparer(inv:Invocation, method:String)(comparer:(PType[_], ElemValue, ElemValue) => Boolean):QFut = {
    for {
      first <- inv.processParamNofM(0, 2)
      second <- inv.processParamNofM(1, 2)
    }
      yield boolean2YesNoQValue(compareValues(first, second, method)(comparer))
  }
  
  lazy val EqualsMethod = new InternalMethod(EqualsMethodOID,
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
          |This receives a VALUE, and tells you whether it matches the given EXP.""".stripMargin)))
  {  
    override def qlApply(inv:Invocation):QFut = {
      binaryComparer(inv, "_equals")( (pt, elem1, elem2) => pt.matches(elem1, elem2) )
    }
  }
  
  lazy val LessThanMethod = new InternalMethod(LessThanMethodOID,
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
          |This receives a VALUE, and tells you whether it is less than the given EXP.""".stripMargin)))
  {  
    override def qlApply(inv:Invocation):QFut = {
      binaryComparer(inv, "_lessThan")( (pt, elem1, elem2) => pt.comp(inv.context)(elem1, elem2) )
    }
  }
  
  lazy val GreaterThanMethod = new InternalMethod(GreaterThanMethodOID,
    toProps(
      setName("_greaterThan"),
      Categories(LogicTag),
      Summary("Is the first parameter greater than the second?"),
      Details("""```
          |_greaterThan(EXP1, EXP2) -> YES OR NO
          |```
          |_greaterThan produces Yes iff the value in the first expression is greater than the second. The definition
          |of "greater than" is type-dependent, but by and large is similar to > in most programming languages.
          |
          |Alternate version:
          |```
          |VALUE -> _greaterThan(EXP) -> YES OR NO
          |```
          |This receives a VALUE, and tells you whether it is greater than the given EXP.""".stripMargin)))
  {  
    override def qlApply(inv:Invocation):QFut = {
      binaryComparer(inv, "_greaterThan") { (pt, elem1, elem2) =>
        // This one's slightly complex. pt.comp() returns false iff the second value is greater than *or* equal to
        // the first. So we need to also do an equality comparison.
        !pt.comp(inv.context)(elem1, elem2) && !pt.matches(elem1, elem2) 
      }
    }
  }
  
  /**
   * Goes through a parameter list full of YesNo expressions, and returns the results.
   */
  private def computeBooleans(inv:Invocation):InvocationValue[Boolean] = {
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
      }
        yield paramResult
  }
  
  lazy val OrMethod = new InternalMethod(OrMethodOID,
    toProps(
      setName("_or"),
      Categories(LogicTag),
      Summary("Produces true iff any of the parameters are true"),
      Details("""```
          |_if(_or(VAL1, VAL2, VAL3...), RESULTS)
          |```
          |_or takes one or more parameters, and produces true if and only if at least one of the
          |elements in one of those parameters is true.""".stripMargin)))
  {
    override def qlApply(inv:Invocation):QFut = {
      val results = computeBooleans(inv)
        
      results.get.map(_.exists(v => v))
    }
  }
  
  lazy val AndMethod = new InternalMethod(AndMethodOID,
    toProps(
      setName("_and"),
      Categories(LogicTag),
      Summary("Produces true iff all of the parameters are true"),
      Details("""```
          |_if(_and(VAL1, VAL2, VAL3...), RESULTS)
          |```
          |_and takes one or more parameters, and produces true if and only if all of the elements in
          |all of those parameters are true.""".stripMargin)))
  {
    override def qlApply(inv:Invocation):QFut = {
      val results = computeBooleans(inv)
        
      results.get.map(r => !(r.exists(v => !v)))
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
  lazy val PlusMethod = new AbstractFunction(AddMethodOID, Received,
    toProps(
      setName("_plus"),
      Categories(LogicTag),
      Summary("Add two values together"),
      Details("""    VALUE -> _plus(OTHER) -> RESULT
        |This pretty much does what you would expect. However, note that it is only implemented for one
        |or two Types yet. If you have a case where you expect and need _plus to work, please drop us a note!""".stripMargin)))
  
  lazy val plusNumericImpl = new FunctionImpl(AddNumericMethodOID, PlusMethod, Seq(IntType, Core.FloatType, Core.LongType))
  {
    override def qlApply(inv:Invocation):QFut = {
      // This is a bit odd-looking, but we need to extract pt.nType for the rest to work:
      val pt = inv.context.value.pType.asInstanceOf[querki.core.IntTypeBasis#NumericTypeBase[_]]
      for {
        typ <- inv.contextTypeAs[querki.core.IntTypeBasis#NumericTypeBase[pt.nType]]
        n <- inv.contextAllAs(typ)
        m <- inv.processParamFirstAs(0, typ)
        result = typ.numeric.plus(n, m)
      }
        yield ExactlyOne(typ(result))
    }
  }
  
  lazy val MinusMethod = new AbstractFunction(SubtractMethodOID, Received,
    toProps(
      setName("_minus"),
      Categories(LogicTag),
      Summary("Subtract one value from another"),
      Details("""    VALUE -> _minus(OTHER) -> RESULT
        |This pretty much does what you would expect. However, note that it is only implemented for one
        |or two Types yet. If you have a case where you expect and need _minus to work, please drop us a note!""".stripMargin)))
  
  lazy val minusNumericImpl = new FunctionImpl(SubtractNumericMethodOID, MinusMethod, Seq(IntType, Core.FloatType, Core.LongType))
  {
    override def qlApply(inv:Invocation):QFut = {
      // This is a bit odd-looking, but we need to extract pt.nType for the rest to work:
      val pt = inv.context.value.pType.asInstanceOf[querki.core.IntTypeBasis#NumericTypeBase[_]]
      for {
        typ <- inv.contextTypeAs[querki.core.IntTypeBasis#NumericTypeBase[pt.nType]]
        n <- inv.contextAllAs(typ)
        m <- inv.processParamFirstAs(0, typ)
        result = typ.numeric.minus(n, m)
      }
        yield ExactlyOne(typ(result))
    }
  }
  
  lazy val TimesMethod = new AbstractFunction(TimesMethodOID, Received,
    toProps(
      setName("_times"),
      Categories(LogicTag),
      Summary("Multiple two values together"),
      Details("""    VALUE -> _times(OTHER) -> RESULT
        |This pretty much does what you would expect. However, note that it is only implemented for one
        |or two Types yet. If you have a case where you expect and need _times to work, please drop us a note!""".stripMargin)))
  
  lazy val timesNumericImpl = new FunctionImpl(TimesNumericMethodOID, TimesMethod, Seq(IntType, Core.FloatType, Core.LongType))
  {
    override def qlApply(inv:Invocation):QFut = {
      // This is a bit odd-looking, but we need to extract pt.nType for the rest to work:
      val pt = inv.context.value.pType.asInstanceOf[querki.core.IntTypeBasis#NumericTypeBase[_]]
      for {
        typ <- inv.contextTypeAs[querki.core.IntTypeBasis#NumericTypeBase[pt.nType]]
        n <- inv.contextAllAs(typ)
        m <- inv.processParamFirstAs(0, typ)
        result = typ.numeric.times(n, m)
      }
        yield ExactlyOne(typ(result))
    }
  }
  
  lazy val DivideMethod = new AbstractFunction(DivideMethodOID, Received,
    toProps(
      setName("_divideBy"),
      Categories(LogicTag),
      Summary("Divide one value by another"),
      Details("""    VALUE -> _divideBy(OTHER) -> RESULT
        |This pretty much does what you would expect. However, note that it is only implemented for one
        |or two Types yet. If you have a case where you expect and need _divideBy to work, please drop us a note!""".stripMargin)))
  
  lazy val divideNumericImpl = new FunctionImpl(DivideNumericMethodOID, DivideMethod, Seq(IntType, Core.FloatType, Core.LongType))
  {
    override def qlApply(inv:Invocation):QFut = {
      // This is a bit odd-looking, but we need to extract pt.nType for the rest to work:
      val pt = inv.context.value.pType.asInstanceOf[querki.core.IntTypeBasis#NumericTypeBase[_]]
      for {
        typ <- inv.contextTypeAs[querki.core.IntTypeBasis#NumericTypeBase[pt.nType]]
        n <- inv.contextAllAs(typ)
        m <- inv.processParamFirstAs(0, typ)
        result = typ.numeric.toDouble(n) / typ.numeric.toDouble(m)
      }
        yield ExactlyOne(Core.FloatType(result))
    }
  }

  
  override lazy val props = Seq(
    new FirstNonEmptyMethod,
    new NotMethod,
    new IfMethod,
    EqualsMethod,
    LessThanMethod,
    GreaterThanMethod,
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

  /******************************************
   * THINGS
   ******************************************/
  
  class BooleanThingOps(v:BooleanValue) extends ThingOps(v) {
    override def qlApply(inv:Invocation):QFut = Future.successful(v.v)    
  }
  class BooleanValue(tid:OID, elem:ElemValue, pm:PropMap) extends ThingState(tid, systemOID, RootOID, pm)
  {
    val v = ExactlyOne(elem)
    override def thingOps(e:Ecology) = new BooleanThingOps(this)
  }
  
  lazy val trueVal = new BooleanValue(TrueOID, True,
    toProps(
      setName("True"),
      Categories(LogicTag),
      Summary("""The literal true value, for use in QL expressions.""")))
  
  lazy val falseVal = new BooleanValue(FalseOID, False,
    toProps(
      setName("False"),
      Categories(LogicTag),
      Summary("""The literal false value, for use in QL expressions.""")))

  override lazy val things = Seq(
    trueVal,
    falseVal
  )
}
