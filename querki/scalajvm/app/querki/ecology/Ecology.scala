package querki.ecology

import scala.reflect.ClassTag

import querki.values.SpaceState

/**
 * Definition of the Ids for an Ecot.
 * 
 * The ecotId parameter at the top is a global, and must be unique for each Ecot. The master
 * list of these is defined in querki.system.
 * 
 * This object should be defined at the package level, as part of the Ecot's API, so that
 * external systems can use these IDs safely, without causing accidental initialization of
 * the Ecot.
 */
class EcotIds(val ecotId:Short) {
  import models.OID

  /**
   * Defines an OID at the root system level. This is mainly exposed for older legacy Things,
   * that were defined before we began to break the world into Ecots. New Things should
   * *NEVER* use this -- use moid() instead!
   */
  def sysId(local:Int) = OID(0, local)
  
  /**
   * The OID for an Ecot-local Thing.
   * 
   * Each Ecot is required to declare the OIDs for the Things it defines, using this method.
   * 
   * moids should be permanent, just like the OIDs in SystemSpace. These are hardcoded values
   * that will be used in the database, so they *MUST* not change. If you need major changes,
   * deprecate the old value and introduce a new one.
   * 
   * You have 16 bits of namespace per Ecot. The theory is that that should be plenty for
   * any foreseeable Ecot. (Hopefully I won't regret this decision, but Modules aren't
   * supposed to be large.)
   * 
   * (Why "moid"? Ecots were originally called Modules, hence the "m", and it wasn't worth changing.)  
   */
  def moid(localId:Short):OID = {
    sysId((ecotId << 16) + localId)
  }
  
  /**
   * The old, broken algorithm for calculating moids. This was a *horrible* bug, and wound
   * up polluting the OID space for a couple dozen Things. The only saving grace is that this
   * error winds up with the lower 16 bits empty, so the results can't collide with correctly-formed
   * moids.
   * 
   * TODO: go through the existing Spaces, and rewrite all references to these old moids to new
   * ones that are correct. This is going to be delicate work.
   */
  def oldMoid(localId:Short):OID = {
    sysId(ecotId << 16 + localId)
  }
}
object SystemIds extends EcotIds(0) {
  val systemOID = sysId(0)
}

/**
 * Marker trait for classes (usually Actors) that require asynchronous initialization, and which
 * will be register using regAsyncInit().
 */
trait AsyncInitter

trait EcotImpl extends Ecot {
  
  import scala.reflect.runtime.{universe => ru}
  import scala.reflect.runtime.universe._

  private def mirror(clazz:Class[_]):Mirror = ru.runtimeMirror(clazz.getClassLoader)
  private def getType[T](clazz: Class[T]):Type = {
    mirror(clazz).classSymbol(clazz).toType
  }
  private def getClass(tpe:Type):Class[_] = {
    mirror(this.getClass()).runtimeClass(tpe.typeSymbol.asClass)
  }
  
  /**
   * This is the set of all EcologyInterfaces that this Ecot implements.
   */
  def implements:Set[Class[_]] = {
    val markerTpe = getType(classOf[EcologyInterface])
    val markerName = markerTpe.toString()
    val clazz = this.getClass()
    val tpe = getType(clazz)
    // A base class is an EcologyInterface iff its *second* base class is EcologyInterface.
    // (Which means that that is the first thing it is extending.)
    val interfaceSymbols = tpe.baseClasses.filter { possibleInterfaceSymbol =>
      val possibleInterface = possibleInterfaceSymbol.typeSignatureIn(tpe)
      val interfaceBases = possibleInterface.baseClasses
      // Comparing by name is a bit lame, but I haven't yet figured out how to reliably
      // compare by identity:
      (interfaceBases.length > 1 &&
       interfaceBases(1).fullName == markerName)
    }
    interfaceSymbols.map(interface => mirror(clazz).runtimeClass(interface.asClass)).toSet
  }
  
  override def addState(prevState:SpaceState):SpaceState = addSystemObjects(prevState)
  
  /**
   * If the Module requires any specialized Things, Properties, Types or Collections,
   * add them to the state here. The returned state should be the passed-in one plus
   * the Module's stuff. (Note that the passed-in state is the System Space -- you are
   * adding stuff to System.)
   * 
   * Individual Modules should not usually need to override this; instead, just define
   * your objects in the types, props and things properties, and they will be added
   * automatically.
   */
  def addSystemObjects(state:SpaceState):SpaceState
  
  /**
   * If this Ecot manages any Actors, they should be created in here. This is called after
   * main Ecology initialization is complete, but there is currently no mechanism for
   * dependencies between the Actors at setup time! We'll add that if we need to.
   * 
   * The createActorCb contains the ActorContext to use for creating the Actor; Ecots should
   * use this instead of any other mechanism for creating it, so that the Actor is hooked into
   * the overall ActorSystem properly.
   */
  def createActors(createActorCb:CreateActorFunc):Unit = {}
  
  /**
   * Ecots should override this in order to declare their persisted messages. Use the
   * persist() convenience function to declare them.
   * 
   * Note that this is *required* for any persisted messages, which should also be marked
   * as UseKryo!
   */
  def persistentMessages:(Short, Seq[(Class[_], Short)]) = (-1, Seq.empty)
  
  protected def persist(ecotId:Short, decls:(Class[_], Short)*) = (ecotId, decls)
  
  protected def regAsyncInit[T <: AsyncInitter](implicit tag:ClassTag[T]) = {
    val clazz = tag.runtimeClass
    ecology match {
      case impl:EcologyImpl => impl.regAsyncInitter(clazz)
      case _ => // Testing...
    }
  }
}

class EcologyImpl(val playApp:Option[play.api.Application]) 
  extends EcologyImplBase[SpaceState, EcotImpl]
{
  def init(initialSpaceState:SpaceState, createActorCb:CreateActorFunc):SpaceState = {
    val finalState = init(initialSpaceState) { state =>
      collectPersistence()
      initializeActors(createActorCb)
      state
    }
    finalState
  }
  
  private def initializeActors(createActorCb:CreateActorFunc) = {
    _initializedEcots.foreach(_.createActors(createActorCb))
  }
  
  private def collectPersistence() = {
    val msgs = (Seq.empty[(Class[_], Int)] /: _initializedEcots) { (msgs, ecot) =>
      val (ecotId, ecotMsgs) = ecot.persistentMessages
      if (ecotId == -1)
        msgs
      else {
        val adjusted = ecotMsgs.map { case (clazz, msgId) => (clazz, (ecotId << 16) + msgId) }
        msgs ++ adjusted
      }
    }
    
    // HACK: workaround to get this list through to KryoInit:
    querki.persistence.KryoInit.setMsgs(msgs)
  }
  
  /**
   * Side-mechanism for those occasional times when we need to fetch an interface from Play. This is
   * kind of cheating, but helps with the fact that our Ecology mechanism doesn't quite match Play's
   * new (as of 2.4) DI view of the world.
   */
  def playApi[T](implicit tag:ClassTag[T]):T = {
    playApp match {
      case Some(app) => app.injector.instanceOf(tag)
      case _ => throw new Exception(s"Trying to fetch interface ${tag.runtimeClass.getName}, but I don't have an app!")
    }
  }
  
  var asyncInitters:Set[Class[_]] = Set.empty
  // TODO: there must be a way to make this more strongly-typed, but it's eluding me today:
  private [ecology] def regAsyncInitter(clazz:Class[_]) = asyncInitters = asyncInitters + clazz
  // True iff there are AsyncInitters that haven't checked in yet
  def waitingForAsync:Boolean = !asyncInitters.isEmpty
  // Marks the given AsyncInitter as ready
  def gotAsync(clazz:Class[_]) = {
    asyncInitters = asyncInitters - clazz
  }
}

object PlayEcology {
  def playApi[T](implicit tag:ClassTag[T], ecology:Ecology):T = {
    ecology.asInstanceOf[EcologyImpl].playApi(tag)
  }
  
  def maybeApplication(implicit ecology:Ecology):Option[play.api.Application] = {
    ecology.asInstanceOf[EcologyImpl].playApp
  }
}
