package querki.ql

import scala.reflect.ClassTag

import querki.values.SpaceState

import models.{AsOID, Collection, Property, PropertyBundle, PType, Thing, ThingId}

import querki.ecology._
import querki.types.ModelTypeBase
import querki.util._
import querki.values.{QLContext, QValue, SpaceState}

sealed trait SigParam
case class RequiredParam(name:String, pts:PType[_]*) extends SigParam

case class Signature(returnPt:PType[_], required:RequiredParam*) {
  def numRequiredParams = required.length
}

/**
 * Extra information that can get picked up and carried along with the InvocationValue. Pulled out to here,
 * to keep it from clogging up the logic of the main InvocationValueImpl.
 */
private[ql] case class IVMetadata(
    // Allows the Function to declare which PType it expects to return. This allows us to preserve type safety
    // even when we return QNone.
    returnType:Option[PType[_]] = None,
    preferredColl:Option[Collection] = None) 
{
  /**
   * This method is the heart of IVMetadata, allowing us to combine metadatas in flatMap(). The copy
   * should account for all parameters.
   */
  def +(other:IVMetadata) = {
    this.copy(
        returnType = this.returnType orElse other.returnType,
        preferredColl = this.preferredColl orElse other.preferredColl)
  }
}

private[ql] case class InvocationValueImpl[T](inv:Invocation, vs:Iterable[T], errOpt:Option[PublicException] = None, metadata:IVMetadata = IVMetadata())(implicit val ecology:Ecology) 
  extends InvocationValue[T] with EcologyMember
{ self =>
  lazy val QL = interface[QL]
  
  def map[R](f:T => R):InvocationValue[R] = {
    errOpt match {
      // If there has already been an error, just propagate that:
      case Some(err) => InvocationValueImpl[R](inv, None, errOpt, metadata)
      // Otherwise, actually call f:
      case None => {
        val maps = vs.map(v => f(v))
        InvocationValueImpl(inv, maps, None, metadata)
      }
    }
  }
  
  def flatMap[R](f:T => InvocationValue[R]):InvocationValue[R] = {
    errOpt match {
      // If there has already been an error, just propagate that:
      case Some(err) => InvocationValueImpl[R](inv, None, errOpt, metadata)
      // Otherwise, actually call f:
      case None => {
        // This gets a little complex, so that we can preserve interim errors:
        (InvocationValueImpl(inv, Seq.empty[R], None) /: vs) { (current, v) =>
          current.errOpt match {
            case Some(err) => InvocationValueImpl[R](inv, None, current.errOpt, metadata)
            case None => {
              // HACK: is there a really good way of avoiding this cheat without polluting the public API of
              // InvocationValue with the errOpt?
              val result = f(v).asInstanceOf[InvocationValueImpl[R]]
              result.errOpt match {
                case Some(err) => result
                case None => InvocationValueImpl(inv, current.vs ++: result.vs, None, metadata + result.metadata)
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
    def map[R](mf:T => R):InvocationValue[R] = new InvocationValueImpl(inv, self.vs.filter(f), errOpt, metadata).map(mf)
    def flatMap[R](mf:T => InvocationValue[R]):InvocationValue[R] = new InvocationValueImpl(inv, self.vs.filter(f), errOpt, metadata).flatMap(mf)
    def withFilter(g:T => Boolean):WithFilter[T] = new WithFilterImpl((x => f(x) && g(x)))
  }
  def withFilter(f:T => Boolean):WithFilter[T] = new WithFilterImpl(f)
    
  def get:Iterable[T] = vs
  def getError:Option[QValue] = errOpt.map { ex =>
    val msg = ex.display(Some(inv.context.request))
    QL.WarningValue(msg) 
  }
  def getReturnType:Option[PType[_]] = metadata.returnType
  def preferredColl:Option[Collection] = metadata.preferredColl
}

private[ql] case class InvocationImpl(invokedOn:Thing, receivedContext:QLContext, val definingContext:Option[QLContext], paramsOpt:Option[Seq[QLPhrase]], sig:Option[Signature] = None)(implicit val ecology:Ecology) 
  extends Invocation with EcologyMember
{
  lazy val QL = interface[querki.ql.QL]
  lazy val Core = interface[querki.core.Core]
  lazy val Types = interface[querki.types.Types]
  
  lazy val displayName = invokedOn.displayName
  lazy val LinkType = Core.LinkType
  
  def error[VT](name:String, params:String*) = InvocationValueImpl[VT](this, None, Some(PublicException(name, params:_*)))
  
  def returnsType(pt:PType[_]):InvocationValue[Boolean] = {
    InvocationValueImpl(this, Some(true), None, IVMetadata(returnType = Some(pt)))
  }
  
  def preferCollection(coll:Collection):InvocationValue[Boolean] = {
    InvocationValueImpl(this, Some(true), None, IVMetadata(preferredColl = Some(coll)))
  }
  
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
  
  def contextValue:InvocationValue[QValue] = {
    InvocationValueImpl(this, Some(context.value), None)
  }
  
  def wrap[T](v:T):InvocationValue[T] = {
    InvocationValueImpl(this, Some(v), None)
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
  
  def contextAllBundles:InvocationValue[PropertyBundle] = {
    if (context.value.matchesType(Core.LinkType)) {
      val ids = context.value.flatMap(Core.LinkType)(Some(_))
      val thingsOpt = ids.map(state.anything(_))
      if (thingsOpt.forall(_.isDefined))
        InvocationValueImpl(this, thingsOpt.flatten, None)
      else
        error("Func.unknownThing", displayName)      
    } else context.value.pType match {
      case mt:ModelTypeBase => {
        val bundles = context.value.flatMap(mt)(Some(_))
        InvocationValueImpl(this, bundles, None)
      }
      case _ => error("Func.notThing", displayName)
    } 
  }
  
  def contextBundlesAndContexts:InvocationValue[(PropertyBundle, QLContext)] = {
    if (context.value.matchesType(Core.LinkType)) {
      val ids = context.value.flatMap(Core.LinkType)(Some(_))
      val thingsOpt = ids.map(state.anything(_))
      if (thingsOpt.forall(_.isDefined))
        InvocationValueImpl(this, thingsOpt.flatten.map(t => (t, context.next(Core.ExactlyOne(Core.LinkType(t))))), None)
      else
        error("Func.unknownThing", displayName)      
    } else context.value.pType match {
      case mt:ModelTypeBase => {
        val pairs = context.value.cv.map(elem => (elem.get(mt), context.next(Core.ExactlyOne(elem))))
        InvocationValueImpl(this, pairs, None)
      }
      case _ => error("Func.notThing", displayName)
    } 
  }
  
  def bundlesAndContextsForProp(prop:Property[_,_]):InvocationValue[(PropertyBundle, QLContext)] = {
    
    def wrapContexts(bundle:PropertyBundle):Iterable[(PropertyBundle, QLContext)] = {
      context.value.cv.map(elem => (bundle, context.next(Core.ExactlyOne(elem))))
    }
    
    def withLexicalContext:Option[PropertyBundle] = {
      for {
        parser <- context.parser
        lex <- parser.lexicalThing
      }
        yield lex
    }

    // Note that the elemContexts returned here are the same as the bundles. It isn't strictly clear that
    // that is correct -- conceptually, when we've walked back up the stack, we should be using the received
    // context as the elemContexts, I think. But things get weirdly multiplicative when we do that, and I'm
    // not sure how to correctly tame that.
    def withCurrentContext(current:QLContext):Option[InvocationValue[(PropertyBundle, QLContext)]] = {
      if (current.value.matchesType(Core.LinkType)) {
        val ids = current.value.flatMap(Core.LinkType)(Some(_))
        val thingsOpt = ids.map(state.anything(_))
        val things = thingsOpt.flatten
        val pairs = things.map(t => (t, context.next(Core.ExactlyOne(Core.LinkType(t)))))
        if (things.exists(_.hasProp(prop)))
          Some(InvocationValueImpl(this, pairs, None))
        else
          None
      } else current.value.pType match {
        case mt:ModelTypeBase => {
          val pairs = current.value.cv.map(elem => (elem.get(mt), context.next(Core.ExactlyOne(elem))))
          if (pairs.exists(_._1.hasProp(prop)))
            Some(InvocationValueImpl(this, pairs, None))
          else
            None
        }
        case _ => None
      }
    }

    // Walk recursively back the Context chain, until we find one that was a Thing (that is, a LinkType), or
    // we run out of Contexts. Note that we start with the one that was *already* tried, because we need to
    // inject lexical checking into this pathway:
    def walkNonThingContexts(previous:QLContext):InvocationValue[(PropertyBundle, QLContext)] = {
      if (previous.parentOpt.isEmpty || previous.value.matchesType(LinkType))
        // Either we hit the end of the chain, or we hit a Link -- either way, time to stop:
        InvocationValueImpl(this, None, None, IVMetadata(returnType = Some(LinkType)))
      else {
        // Keep walking back up the chain, to see if we find something:
        val current = previous.parent
        withCurrentContext(current) match {
          case Some(result) => result
          case None => walkNonThingContexts(current)
        }
      }
    }
    
    if (definingContext.isDefined) {
      // If there is a defining context, that is where we should be working from:
      val result = for {
        dc <- definingContext
        id <- dc.value.firstAs(Core.LinkType)
        thing <- state.anything(id)
      }
        yield InvocationValueImpl(this, wrapContexts(thing), None)
        
      result.getOrElse(error("Func.notThing", displayName))
    } else {
      // No defining context -- does the received context have the property?
      withCurrentContext(context) match {
        case Some(result) => result
        case None => {
          // Nope. Does the lexical context?
          withLexicalContext match {
            // IMPORTANT SUBTLETY: note that we're passing through the *bundle* of the lexical context, but the actual contexts are the received context!
            case Some(bundle) if (bundle.hasProp(prop)) => InvocationValueImpl(this, wrapContexts(bundle), None)
            case _ => {
              // If this bundle isn't a Thing, walk up the context chain.
              walkNonThingContexts(context)
            }
          }          
        }
      }
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
  
  // TODO: merge these two functions. They're mostly alike...
  def processParam(paramNum:Int, processContext:QLContext = context):InvocationValue[QValue] = {
    paramsOpt match {
      case Some(params) if (params.length >= (paramNum - 1)) => {
        val processed = context.parser.get.processPhrase(params(paramNum).ops, processContext).value
        processed.firstAs(QL.ErrorTextType) match {
          // If there was an error, keep the error, and stop processing:
          case Some(errorText) => InvocationValueImpl(this, None, Some(new PublicException("General.public", errorText))) 
          case None => InvocationValueImpl(this, Some(processed), None)
        }
      }
      case _ => error("Func.missingParam", displayName)
    }    
  }
  
  def processParamFirstAs[VT](paramNum:Int, pt:PType[VT], processContext:QLContext = context):InvocationValue[VT] = {
    paramsOpt match {
      case Some(params) if (params.length >= (paramNum - 1)) => {
        val processed = context.parser.get.processPhrase(params(paramNum).ops, processContext).value
        processed.firstAs(QL.ErrorTextType) match {
          case Some(errorText) => InvocationValueImpl(this, None, Some(new PublicException("General.public", errorText)))
          case None => {
	        processed.firstAs(pt) match {
	          case Some(v) => InvocationValueImpl(this, Some(v), None)
	          case None => error("Func.paramNotThing", displayName, paramNum.toString)
	        }
          }
        }
      }
      case _ => error("Func.missingParam", displayName)
    }
  }
  
  def firstParamOrContextValue:InvocationValue[QValue] = {
    processParamNofM(0, 1)
  }
  
  def processParamNofM(paramNum:Int, expectedParams:Int, processContext:QLContext = context):InvocationValue[QValue] = {
    if (numParams < (expectedParams - 1))
      InvocationValueImpl(this, None, Some(new PublicException("Func.insufficientParams", displayName, (expectedParams - 1))))
    else {
      if (numParams >= expectedParams)
        processParam(paramNum, processContext)
      else if (paramNum == 0)
        contextValue
      else
        processParam(paramNum - 1, processContext)
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
