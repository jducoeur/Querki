package querki.system

import akka.actor._
import akka.cluster.Cluster
import akka.cluster.sharding._
import akka.cluster.singleton._

import querki.ecology._
import querki.values.SpaceState

/**
 * The interface to manage the System.
 */
trait SystemManagement extends EcologyInterface {
  /**
   * Set the final System Space. The code that initialized the Ecology *must* call this once complete!
   */
  def setState(state:SpaceState)
  
  /**
   * The ActorSystem, if it exists. Note that it does *not* exist in many unit tests, and thus
   * createActors() will not be called on the Ecots!
   */
  def actorSystemOpt:Option[ActorSystem]
  
  /**
   * The one true ActorSystem that everybody is living in. Ecots should obtain the ActorSystem through this,
   * if they need it.
   * 
   * Note the explicit assumption that there is a 1-to-1 correspondence between the Ecology and the
   * ActorSystem.
   * 
   * This will throw an exception if called when there is no ActorSystem, as in unit tests! As a consequence, you
   * should *NEVER* use this during initialization of a normal Ecot. If you find that you need to, it probably
   * means that whatever you're doing needs to get refactored into SystemManagement instead.
   */
  def actorSystem:ActorSystem
  
  /**
   * The address of this Cluster. Mainly intended for monitoring.
   */
  def clusterAddress:String
  
  /**
   * As it says, this is a wrapper around the standard ShardRegion creation, pulled out to here so that
   * it can be stubbed for unit testing.
   */
  def createShardRegion(name:String, props:Props, identityExtractor:ShardRegion.ExtractEntityId, identityResolver:ShardRegion.ExtractShardId):Option[ActorRef]
  
  /**
   * Sets up a Cluster Singleton. (Which will be stubbed if we're in a unit-test environment.)
   */
  def createClusterSingleton(createActorCb:CreateActorFunc, props:Props, singletonName:String, proxyName:String, terminationMsg:Any):(Option[ActorRef], Option[ActorRef])
  
  /**
   * The Actor that AsyncInitters should tell when they are ready.
   */
  def asyncInitTarget:ActorRef
}

object SystemMOIDs extends EcotIds(18)

class SystemEcot(e:Ecology, val actorSystemOpt:Option[ActorSystem], val asyncInitTarget:ActorRef) extends QuerkiEcot(e) with System with SystemManagement {
  
  lazy val Apps = interface[querki.apps.Apps]
  lazy val Basic = interface[querki.basic.Basic]
  lazy val Conventions = interface[querki.conventions.Conventions]
  lazy val Tags = interface[querki.tags.Tags]
            
  val defaultSpaceRootView = """{{well well-sm _root-well:
    |{{row:
    |{{col-md-3:
    |### Pages
    |}}
    |
    |{{col-md-9:
    |[[Simple Thing -> _children(_space = $_context) -> _filter(_not(_isModel)) -> _sort -> ""
    |#### ____""]]
    |
    |[[Simple Thing -> _if(_hasPermission(Who Can Create._self), _createButton(""Write a Page"", classes=""btn-xs btn-primary""))]]
    |}}
    |}}
    |}}
    |
    |
    |{{well well-sm _root-well:
    |{{row:
    |{{col-md-3:
    |### Things
    |}}
    |
    |{{col-md-9:
    |[[_allThings -> _filter(_isModel) -> _sort -> _showModelTree]]
    |
    |[[Simple Thing -> _if(_hasPermission(Who Can Design._self), _menuButton(""designAModel"", ""Design a New Model"", class=""btn-xs btn-primary""))]]
    |}}
    |}}
    |}}
    |
    |
    |[[Tag Type -> 
    |  _propsOfType(_space = $_context) ->
    |  _sort ->
    |*""{{well well-sm _root-well:
    |{{row:
    |{{col-md-3:
    |### Tags
    |}}
    |
    |{{col-md-9:
    |[[_foreach(""**____**: [[_tagsForProperty(_space = $_context) -> _sort -> _join("", "")]]
    |"")]]
    |}}
    |}}
    |}}
    |""
    |]]
    """.stripMargin
  
  // This is called when the world finishes up:
  def setState(stateIn:SpaceState) = {
    // Now that all of the Properties exist, we can modify the System Space to use them:
    val state = stateIn.copy(pf = 
      toProps(
        setName("System"),
        Basic.DisplayTextProp(defaultSpaceRootView),
        Tags.ShowUnknownProp(querki.tags.defaultDisplayText),
        Apps.GallerySummary(),
        Apps.GalleryDetails()))
    _state = Some(state)
  }
  
  var _state:Option[SpaceState] = None
  def State = _state.getOrElse(throw new Exception("Attempting to access the System Space before init is complete!"))
  
  def actorSystem = actorSystemOpt.get
  
  def createShardRegion(name:String, props:Props, identityExtractor:ShardRegion.ExtractEntityId, identityResolver:ShardRegion.ExtractShardId) = {
    Some(ClusterSharding(actorSystem).start(
        typeName = name, 
        entityProps = props, 
        settings = ClusterShardingSettings(actorSystem),
        extractEntityId = identityExtractor, 
        extractShardId = identityResolver))
  }
  
  def clusterAddress:String = {
    val cluster = Cluster.get(actorSystem)
    cluster.selfAddress.toString
  }
  
  def createClusterSingleton(createActorCb:CreateActorFunc, props:Props, singletonName:String, proxyName:String, terminationMsg:Any):(Option[ActorRef], Option[ActorRef]) = {
    actorSystemOpt match {
      case Some(system) => {
        val manager = createActorCb(ClusterSingletonManager.props(
            props,
            terminationMsg,
            ClusterSingletonManagerSettings(system)
          ),
          singletonName)
        val proxy = createActorCb(ClusterSingletonProxy.props(
            s"/user/querkiRoot/$singletonName",
            ClusterSingletonProxySettings(system)),
          proxyName)
          
        (manager, proxy)
      }
      case _ => (Some(ActorRef.noSender), Some(ActorRef.noSender))
    }

  }
}
