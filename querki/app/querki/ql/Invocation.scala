package querki.ql

import scala.reflect.ClassTag

import querki.values.SpaceState

import models.{PType, Thing}

import querki.ecology._
import querki.values.{QLContext, QValue, SpaceState}

sealed trait SigParam
case class RequiredParam(name:String, pts:PType[_]*) extends SigParam

case class Signature(returnPt:PType[_], required:RequiredParam*) {
  def numRequiredParams = required.length
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
  
  def processParam(paramNum:Int, processContext:QLContext = context):QValue = {
    if (paramsOpt.isEmpty || (paramsOpt.get.length < paramNum + 1))
      throw new Exception(s"Bad invocation of processParam in ${invokedOn.displayName} -- doesn't appear to have checked number of parameters!")
    parser.get.processPhrase(paramsOpt.get(paramNum).ops, processContext).value
  }
}
