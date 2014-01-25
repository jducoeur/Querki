package querki.ql

import scala.reflect.ClassTag

import querki.values.SpaceState

import models.{PType, Thing}

import querki.ecology._
import querki.util._
import querki.values.{QLContext, QValue, SpaceState}

sealed trait SigParam
case class RequiredParam(name:String, pts:PType[_]*) extends SigParam

case class Signature(returnPt:PType[_], required:RequiredParam*) {
  def numRequiredParams = required.length
}

private[ql] case class InvocationValueImpl[T](inv:Invocation, vOpt:Option[T] = None, errOpt:Option[PublicException] = None)(implicit val ecology:Ecology) 
  extends InvocationValue[T] with EcologyMember
{
  lazy val QL = interface[QL]
  
  def map[R](f:T => R):InvocationValue[R] = {
    errOpt match {
      // If there has already been an error, just propagate that:
      case Some(err) => InvocationValueImpl[R](inv, None, errOpt)
      // Otherwise, actually call f:
      case None => {
        vOpt match {
          case Some(v) => InvocationValueImpl(inv, Some(f(v)), None)
          case None => {
            QLog.error("Somehow got an InvocationValueImpl with no initial value!")
            InvocationValueImpl(inv, None, Some(UnexpectedPublicException))
          }
        }
      }
    }
  }
  
  def flatMap[R](f:T => InvocationValue[R]):InvocationValue[R] = {
    errOpt match {
      // If there has already been an error, just propagate that:
      case Some(err) => InvocationValueImpl[R](inv, None, errOpt)
      // Otherwise, actually call f:
      case None => {
        vOpt match {
          case Some(v) => f(v)
          case None => {
            QLog.error("Somehow got an InvocationValueImpl with no initial value!")
            InvocationValueImpl(inv, None, Some(UnexpectedPublicException))
          }
        }
      }
    }
  }
    
  def get:T = vOpt.get
  def getError:Option[QValue] = errOpt.map { ex =>
    val msg = ex.display(Some(inv.context.request))
    QL.WarningValue(msg) 
  }
}

/**
 * TODO: okay, fine, I give -- this *really* needs to be rewritten as a proper Monad. I want to be able to
 * build a for statement that is basically checking this Invocation over and over again, returning a WarningValue
 * if anything fails or the resulting value if it all works. That is classic Monadic behaviour, so I should
 * just suck it up and figure it out.
 */
private[ql] case class InvocationImpl(invokedOn:Thing, receivedContext:QLContext, paramsOpt:Option[Seq[QLPhrase]], sig:Option[Signature] = None)(implicit val ecology:Ecology) 
  extends Invocation with EcologyMember
{
  lazy val QL = interface[querki.ql.QL]
  lazy val Core = interface[querki.core.Core]
  
  lazy val displayName = invokedOn.displayName
  
  def error[VT](name:String, params:String*) = InvocationValueImpl[VT](this, None, Some(PublicException(name, params:_*)))
  
  def contextTypeAs[T : ClassTag]:InvocationValue[T] = {
    val clazz = implicitly[ClassTag[T]].runtimeClass
    if (clazz.isInstance(context.value.pType))
      InvocationValueImpl(this, Some(context.value.pType.asInstanceOf[T]), None)
    else
      error("Func.wrongType", displayName)
  }
  
  def contextFirstAs[VT](pt:PType[VT]):InvocationValue[VT] = {
    context.value.firstAs(pt) match {
      case Some(v) => InvocationValueImpl(this, Some(v), None)
      case None => error("Func.notThing", displayName)
    }
  }
  
  def contextFirstThing:InvocationValue[Thing] = {
    contextFirstAs(Core.LinkType).flatMap { oid =>
      state.anything(oid) match {
        case Some(thing) => InvocationValueImpl(this, Some(thing), None)
        case None => error("Func.unknownThing", displayName)
      }
    }
  }
  
  def processParamFirstAs[VT](paramNum:Int, pt:PType[VT], processContext:QLContext = context):InvocationValue[VT] = {
    paramsOpt match {
      case Some(params) if (params.length >= (paramNum - 1)) => {
        val processed = context.parser.get.processPhrase(params(paramNum).ops, context).value
        processed.firstAs(pt) match {
          case Some(v) => InvocationValueImpl(this, Some(v), None)
          case None => error("Func.paramNotThing", displayName, paramNum.toString)
        }
      }
      case _ => error("Func.missingParam", displayName)
    }
  }
  
  def WarningValue(msg:String) = QL.WarningValue(msg)
  
  /**
   * The "primary" context of this invocation. This is an exact synonym for receivedContext, and is the
   * one you usually care about. 
   */
  def context:QLContext = receivedContext
    
  implicit def state:SpaceState = context.state
  
  def parser = context.parser
    
  def numParams:Int = paramsOpt match {
    case Some(params) => params.length
    case None => 0
  }
  
  def ifParamsMismatch(sig:Signature):Option[QValue] = {
    if (numParams < sig.numRequiredParams)
      Some(WarningValue(s"${invokedOn.displayName} requires at least ${sig.numRequiredParams} parameters"))
    else
      None
  }
  
  def ifMatches(sig:Signature)(f:Invocation => QValue):QValue = {
    ifParamsMismatch(sig).getOrElse(f(this.copy(sig = Some(sig))))
  }
  
  def contextAs[T : ClassTag](f:T => QValue):QValue = {
    val clazz = implicitly[ClassTag[T]].runtimeClass
    if (clazz.isInstance(context.value.pType))
      f(context.value.pType.asInstanceOf[T])
    else
      WarningValue(s"${invokedOn.displayName} didn't receive the expected type")
  }
  
  def oldProcessParam(paramNum:Int, processContext:QLContext = context):QValue = {
    if (paramsOpt.isEmpty || (paramsOpt.get.length < paramNum + 1))
      throw new Exception(s"Bad invocation of processParam in ${invokedOn.displayName} -- doesn't appear to have checked number of parameters!")
    parser.get.processPhrase(paramsOpt.get(paramNum).ops, processContext).value
  }
}
