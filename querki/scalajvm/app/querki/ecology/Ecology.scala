package querki.ecology

import scala.reflect.ClassTag

import querki.values.SpaceState

object Ecology {
  private var _theEcology:Option[Ecology] = None
  /**
   * The One True Pointer to the Ecology.
   * 
   * The Ecology system was originally lovely and pure, requiring you to pass the Ecology pointer into
   * all sorts of things. That turns out to be a real hassle in a distributed environment, since the
   * Ecology is absolutely not serializable, and we want to use it in various case classes.
   */
  lazy val ecology = _theEcology.get
  
  def setEcology(e:Ecology) = {
    _theEcology match {
      case Some(existing) => throw new Exception("Trying to set the Ecology a second time!")
      case None => _theEcology = Some(e)
    }
  }
}

trait EcologyMember extends EcologyMemberBase[SpaceState, EcotImpl] {
  implicit def ecology = Ecology.ecology
}

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
}

class EcologyImpl extends EcologyImplBase[SpaceState, EcotImpl] {

  def init(initialSpaceState:SpaceState, createActorCb:CreateActorFunc):SpaceState = {
    val finalState = init(initialSpaceState) { state =>
      initializeActors(createActorCb)
      state
    }
    finalState
  }
  
  private def initializeActors(createActorCb:CreateActorFunc) = {
    // Only call createActors() iff there is an ActorSystem in the first place:
    val SystemManagement = ecology.api[querki.system.SystemManagement]
    SystemManagement.actorSystemOpt.foreach { actorSystem => 
      _initializedEcots.foreach(_.createActors(createActorCb))
    }
  }  
}
