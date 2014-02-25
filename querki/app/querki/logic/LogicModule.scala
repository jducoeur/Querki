package querki.logic

import models.{OID, ThingState}

import querki.ecology._
import querki.ql.QLPhrase
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
}

/**
 * This module should eventually contain everything pertaining to predicate logic.
 * 
 * For now, most of the stuff that should be here is still in the core System Space,
 * but that should be fixed. This should absorb YesNoType, and all of the predicate-oriented
 * methods.
 */
class LogicModule(e:Ecology) extends QuerkiEcot(e) with YesNoUtils with querki.core.MethodDefs {
  import MOIDs._

  /******************************************
   * FUNCTIONS
   ******************************************/
	
	class FirstNonEmptyMethod extends InternalMethod(FirstNonEmptyMethodOID,
	    toProps(
	      setName("_firstNonEmpty"),
	      Summary("""Produces the first parameter that is not empty."""),
	      Details("""    RECEIVED -> _or(CLAUSE1, CLAUSE2, ...) -> RESULT
	          |_or takes any number of parameters. It runs through each of them, applying the incoming context.
	          |It produces the first one that returns a non-empty result, or None iff all of them come out empty.""".stripMargin)))
	{
	  override def qlApply(inv:Invocation):QValue = {
	    val context = inv.context
	    val paramsOpt = inv.paramsOpt
	    
	    paramsOpt match {
	      case Some(params) => {
	        val result = (Option.empty[QValue] /: params) { (current, phrase) =>
	          current match {
	            case Some(result) => Some(result)
	            case None => {
	              val oneResult = context.parser.get.processPhrase(phrase.ops, context)
	              if (oneResult.value.isEmpty)
	                None
	              else
	                Some(oneResult.value)  
	            }
	          }
	        }
	        // If we got nothing out, then produce an empty list of the incoming type
	        // TBD: this really isn't the correct type to produce -- ideally, the type should
	        // be the one that would be output by the various parameter phrases. How can we
	        // suss that?
	        result.getOrElse(EmptyValue(context.value.pType))
	      }
	      case None => WarningValue("_firstNonEmpty() is meaningless if you don't give it any parameters")
	    }
	  }
	}
	
	class NotMethod extends InternalMethod(NotOID,
	    toProps(
	      setName("_not"),
	      Summary("Returns the reverse of the received value, or the parameter"),
	      Details("""    TRUE/FALSE -> _not -> FALSE/TRUE
	          |or
	          |    RECEIVED -> _not(TRUE/FALSE) -> FALSE/TRUE
	          |
	          |_not takes the parameter if one is given, or the received value if not. It returns True iff that it False, and False if it is anything else""".stripMargin)))
	{
	  override def qlApply(inv:Invocation):QValue = {
	    val context = inv.context
	    val paramsOpt = inv.paramsOpt
	    
	    val inVal = paramsOpt match {
	      case Some(params) if (params.length == 1) => {
	        context.parser.get.processPhrase(params(0).ops, context).value
	      }
	      case _ => context.value
	    }
	    !toBoolean(inVal)
	  }
	}
	
	
	// TODO: this will become clearer and easier to use once we introduce block-syntax parameters.
	class IfMethod extends InternalMethod(IfMethodOID,
	    toProps(
	      setName("_if"),
	      Summary("Choose what to produce, as directed"),
	      Details("""    RECEIVED -> _if(YESNO, IFCLAUSE, ELSECLAUSE) -> ...
	          |_if is one of the basic building blocks of programming. It applies the YESNO phrase to the received context.
	          |If the result is Yes, it applies the IFCLAUSE to the received context and produces that. Otherwise, if there
	          |is an ELSECLAUSE, it applies and produces that, or produces None if there is no ELSECLAUSE.
	          |
	          |The syntax of _if is likely to evolve in the future, to something more like most programming languages. For now,
	          |though, note that IFCLAUSE and ELSECLAUSE are *inside* the parentheses, rather than after them as most languages
	          |have it.""".stripMargin)))
	{
	  override def qlApply(inv:Invocation):QValue = {
	    val context = inv.context
	    val paramsOpt = inv.paramsOpt
	    
	    paramsOpt match {
	      case Some(params) if (params.length > 1) => {
	        val predicatePhrase = params(0)
	        val ifCase = params(1)
	        val predResult = context.parser.get.processPhrase(predicatePhrase.ops, context)
	        predResult.value.firstAs(QL.ErrorTextType) match {
	          case Some(errMsg) => predResult.value
	          case None => {
		        if (toBoolean(predResult.value)) {
		          context.parser.get.processPhrase(ifCase.ops, context).value
		        } else if (params.length > 2) {
		          val elseCase = params(2)
		          context.parser.get.processPhrase(elseCase.ops, context).value
		        } else {
		          // TODO: the type here is chosen arbitrarily, but it *should* be the same type as the ifCase.
		          EmptyValue(Core.UnknownType)
		        }
	          }
	        }
	      }
	      case _ => WarningValue("_if requires at least two parameters.")
	    }
	  }
	}
	
	class EqualsMethod extends InternalMethod(EqualsMethodOID,
	    toProps(
	      setName("_equals"),
	      Summary("Do these parameters match?"),
	      Details("""    _equals(EXP1, EXP2) -> YES OR NO
	          |_equals produces Yes iff the expressions in the two parameters match each other. The definition
	          |of "match" is type-dependent, but by and large is similar to == in most programming languages.
	          |
	          |Note that we are likely to enhance this method in the future, to do more, but this is the
	          |important core functionality.""".stripMargin)))
	{  
	  override def qlApply(inv:Invocation):QValue = {
	    val context = inv.context
	    val paramsOpt = inv.paramsOpt
	    
	    paramsOpt match {
	      case Some(params) if (params.length > 1) => {
	        val first = context.parser.get.processPhrase(params(0).ops, context).value
	        val second = context.parser.get.processPhrase(params(1).ops, context).value
	        if (first.pType != second.pType) {
	          WarningValue("The parameters to _equals must be the same Type: got _equals(" + first.pType.displayName + ", " + second.pType.displayName + ")")
	        } else if (first.size == second.size) {
	          val pt = first.pType
	          val pairs = first.cv.zip(second.cv)
	          pairs.forall(pair => pt.matches(pair._1, pair._2))
	        } else {
	          false
	        }
	      }
	      case _ => WarningValue("_equals requires two parameters")
	    }
	  }
	}
	
  lazy val OrMethod = new InternalMethod(OrMethodOID,
      toProps(
        setName("_or"),
        Summary("Produces true iff any of the parameters are true"),
        Details("""    _if(_or(VAL1, VAL2, VAL3...), RESULTS)
            |_or takes one or more parameters, and produces true if and only if at least one of those parameters
            |is true.""".stripMargin)))
  {
    override def qlApply(inv:Invocation):QValue = {
      val results = for {
        dummy <- inv.returnsType(YesNoType)
        paramNum <- inv.iter(0 until inv.numParams, None)
        paramVal <- inv.processParamFirstAs(paramNum, YesNoType)
      }
        yield paramVal
        
      results.get.exists(v => v)
    }
  }
	
  lazy val AndMethod = new InternalMethod(AndMethodOID,
      toProps(
        setName("_and"),
        Summary("Produces true iff all of the parameters are true"),
        Details("""    _if(_and(VAL1, VAL2, VAL3...), RESULTS)
            |_and takes one or more parameters, and produces true if and only if all of those parameters
            |are true.""".stripMargin)))
  {
    override def qlApply(inv:Invocation):QValue = {
      val results = for {
        dummy <- inv.returnsType(YesNoType)
        paramNum <- inv.iter(0 until inv.numParams, None)
        paramVal <- inv.processParamFirstAs(paramNum, YesNoType)
      }
        yield paramVal
        
      !results.get.exists(v => !v)
    }
  }
	
  override lazy val props = Seq(
    new FirstNonEmptyMethod,
    new NotMethod,
    new IfMethod,
    new EqualsMethod,
    OrMethod,
    AndMethod
  )

  /******************************************
   * THINGS
   ******************************************/
  
  class BooleanValue(tid:OID, elem:ElemValue, pf:PropFetcher) extends ThingState(tid, systemOID, RootOID, pf)(ecology)
  {
    val v = ExactlyOne(elem)
    override def qlApply(inv:Invocation):QValue = v
  }
  
  lazy val trueVal = new BooleanValue(TrueOID, True,
      toProps(
        setName("True"),
        Summary("""The literal true value, for use in QL expressions.""")))
  
  lazy val falseVal = new BooleanValue(FalseOID, False,
      toProps(
        setName("False"),
        Summary("""The literal false value, for use in QL expressions.""")))

  override lazy val things = Seq(
    trueVal,
    falseVal
  )
}