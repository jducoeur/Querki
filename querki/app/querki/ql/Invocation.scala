package querki.ql

import scala.reflect.ClassTag

import querki.values.SpaceState

import models.{AsOID, Property, PropertyBundle, PType, Thing, ThingId}

import querki.ecology._
import querki.types.ModelTypeBase
import querki.util._
import querki.values.{QLContext, QValue, SpaceState}

sealed trait SigParam
case class RequiredParam(name:String, pts:PType[_]*) extends SigParam

case class Signature(returnPt:PType[_], required:RequiredParam*) {
  def numRequiredParams = required.length
}

private[ql] case class InvocationValueImpl[T](inv:Invocation, vs:Iterable[T], errOpt:Option[PublicException] = None)(implicit val ecology:Ecology) 
  extends InvocationValue[T] with EcologyMember
{ self =>
  lazy val QL = interface[QL]
  
  def map[R](f:T => R):InvocationValue[R] = {
    errOpt match {
      // If there has already been an error, just propagate that:
      case Some(err) => InvocationValueImpl[R](inv, None, errOpt)
      // Otherwise, actually call f:
      case None => {
        val maps = vs.map(v => f(v))
        InvocationValueImpl(inv, maps, None)
      }
    }
  }
  
  def flatMap[R](f:T => InvocationValue[R]):InvocationValue[R] = {
    errOpt match {
      // If there has already been an error, just propagate that:
      case Some(err) => InvocationValueImpl[R](inv, None, errOpt)
      // Otherwise, actually call f:
      case None => {
        // This gets a little complex, so that we can preserve interim errors:
        (InvocationValueImpl(inv, Seq.empty[R], None) /: vs) { (current, v) =>
          current.errOpt match {
            case Some(err) => InvocationValueImpl[R](inv, None, current.errOpt)
            case None => {
              // HACK: is there a really good way of avoiding this cheat without polluting the public API of
              // InvocationValue with the errOpt?
              val result = f(v).asInstanceOf[InvocationValueImpl[R]]
              result.errOpt match {
                case Some(err) => result
                case None => InvocationValueImpl(inv, current.vs ++: result.vs, None)
              }
            }
          }
        }
      }
    }
  }

  /**
   * Our implementation of withFilter, for "if" statements in for comprehensions.
   */
  class WithFilterImpl(f:T => Boolean) extends WithFilter[T] {
    def map[R](mf:T => R):InvocationValue[R] = new InvocationValueImpl(inv, self.vs.filter(f), errOpt).map(mf)
    def flatMap[R](mf:T => InvocationValue[R]):InvocationValue[R] = new InvocationValueImpl(inv, self.vs.filter(f), errOpt).flatMap(mf)
    def withFilter(g:T => Boolean):WithFilter[T] = new WithFilterImpl((x => f(x) && g(x)))
  }
  def withFilter(f:T => Boolean):WithFilter[T] = new WithFilterImpl(f)
    
  def get:Iterable[T] = vs
  def getError:Option[QValue] = errOpt.map { ex =>
    val msg = ex.display(Some(inv.context.request))
    QL.WarningValue(msg) 
  }
}

private[ql] case class InvocationImpl(invokedOn:Thing, receivedContext:QLContext, val definingContext:Option[QLContext], paramsOpt:Option[Seq[QLPhrase]], sig:Option[Signature] = None)(implicit val ecology:Ecology) 
  extends Invocation with EcologyMember
{
  lazy val QL = interface[querki.ql.QL]
  lazy val Core = interface[querki.core.Core]
  lazy val Types = interface[querki.types.Types]
  
  lazy val displayName = invokedOn.displayName
  
  def error[VT](name:String, params:String*) = InvocationValueImpl[VT](this, None, Some(PublicException(name, params:_*)))
  
  def preferDefiningContext:Invocation = {
    definingContext match {
      case Some(c) => this.copy(receivedContext = c)
      case None => this.copy(definingContext = Some(receivedContext))
    }
  }
  
  def contextTypeAs[T : ClassTag]:InvocationValue[T] = {
    val clazz = implicitly[ClassTag[T]].runtimeClass
    if (clazz.isInstance(context.value.pType))
      InvocationValueImpl(this, Some(context.value.pType.asInstanceOf[T]), None)
    else
      error("Func.wrongType", displayName)
  }
  
  def contextElements:InvocationValue[QLContext] = {
    if (context.useCollection) {
      InvocationValueImpl(this, Some(context), None)
    } else {
      val contexts = context.value.cv.map(elem => context.next(Core.ExactlyOne(elem)))
      InvocationValueImpl(this, contexts, None)
    }
  }
  
  def opt[T](opt:Option[T], errOpt:Option[PublicException] = None):InvocationValue[T] = {
    opt match {
      case Some(v) => InvocationValueImpl(this, Some(v), None)
      case None => {
        errOpt match {
          case Some(err) => InvocationValueImpl(this, None, Some(err))
          // No error specified if this isn't true, so we're simply passing Empty along:
          case None => InvocationValueImpl(this, None, None)
        }
      }
    }
  }
  
  def iter[T](it:Iterable[T], errOpt:Option[PublicException] = None):InvocationValue[T] = {
    InvocationValueImpl(this, it, errOpt)
  }
  
  def contextFirstAs[VT](pt:PType[VT]):InvocationValue[VT] = {
    context.value.firstAs(pt) match {
      case Some(v) => InvocationValueImpl(this, Some(v), None)
      case None => error("Func.notThing", displayName)
    }
  }
  
  def contextAllAs[VT](pt:PType[VT]):InvocationValue[VT] = {
    if (!context.value.matchesType(pt))
      error("Func.notThing", displayName)
    else {
      val vs = context.value.flatMap(pt)(Some(_))
      InvocationValueImpl(this, vs, None)
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
  
  def contextAllThings:InvocationValue[Thing] = {
    if (!context.value.matchesType(Core.LinkType))
      error("Func.notThing", displayName)
    else {
      val ids = context.value.flatMap(Core.LinkType)(Some(_))
      val thingsOpt = ids.map(state.anything(_))
      if (thingsOpt.forall(_.isDefined))
        InvocationValueImpl(this, thingsOpt.flatten, None)
      else
        error("Func.unknownThing", displayName)
    }    
  }
  
  def contextAllBundles(processContext:QLContext = context):InvocationValue[PropertyBundle] = {
    if (processContext.value.matchesType(Core.LinkType)) {
      val ids = processContext.value.flatMap(Core.LinkType)(Some(_))
      val thingsOpt = ids.map(state.anything(_))
      if (thingsOpt.forall(_.isDefined))
        InvocationValueImpl(this, thingsOpt.flatten, None)
      else
        error("Func.unknownThing", displayName)      
    } else processContext.value.pType match {
      case mt:ModelTypeBase => {
        val bundles = processContext.value.flatMap(mt)(Some(_))
        InvocationValueImpl(this, bundles, None)
      }
      case _ => error("Func.notThing", displayName)
    } 
  }
  
  def definingContextAsProperty:InvocationValue[Property[_,_]] = {
    definingContext match {
      case Some(defining) => {
        if (!defining.value.matchesType(Core.LinkType))
          error("Func.notThing", displayName)
        else {
          val ids:Iterable[ThingId] = defining.value.flatMap(Core.LinkType)(oid => Some(AsOID(oid)))
          val propOpts:Iterable[Option[Property[_,_]]] = ids.map(state.prop(_))
          if (propOpts.forall(_.isDefined)) {
            InvocationValueImpl(this, propOpts.flatten, None)
          } else
            error("Func.notProp", displayName)
    }          
      }
      case None => error("Func.missingDefiningContext", displayName)
    }    
  }
  
  def definingContextAsPropertyOf[VT](targetType:PType[VT]):InvocationValue[Property[VT,_]] = {
    definingContext match {
      case Some(defining) => {
        if (!defining.value.matchesType(Core.LinkType))
          error("Func.notThing", displayName)
        else {
          val ids:Iterable[ThingId] = defining.value.flatMap(Core.LinkType)(oid => Some(AsOID(oid)))
          val propOpts:Iterable[Option[Property[VT,_]]] = ids.map(state.prop(_).flatMap(prop => prop.confirmType(targetType)))
          if (propOpts.forall(_.isDefined)) {
            InvocationValueImpl(this, propOpts.flatten, None)
          } else
            error("Func.notProp", displayName)
    }          
      }
      case None => error("Func.missingDefiningContext", displayName)
    }  
  }
  
  def processParam(paramNum:Int, processContext:QLContext = context):InvocationValue[QValue] = {
    paramsOpt match {
      case Some(params) if (params.length >= (paramNum - 1)) => {
        val processed = context.parser.get.processPhrase(params(paramNum).ops, processContext).value
        InvocationValueImpl(this, Some(processed), None)
      }
      case _ => error("Func.missingParam", displayName)
    }    
  }
  
  def processParamFirstAs[VT](paramNum:Int, pt:PType[VT], processContext:QLContext = context):InvocationValue[VT] = {
    paramsOpt match {
      case Some(params) if (params.length >= (paramNum - 1)) => {
        val processed = context.parser.get.processPhrase(params(paramNum).ops, processContext).value
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
}
