package querki.apps

import querki.globals._
import querki.spaces.messages.SpaceInfo
import querki.util.QuerkiActor

private [apps] trait ExtractAppProgress {
  var msg:String = "Extracting App"
  
  // Set when we are complete
  var appInfo:Option[querki.data.SpaceInfo] = None
  
  // Set to true iff the upload process fails
  var failed = false
}

/**
 * The manager of the extraction process.
 * 
 * This is really just a part of ExtractAppActor, separated out so that we don't conflate
 * the Actor-ness with the processing.
 * 
 * @author jducoeur
 */
private [apps] trait ExtractAppProcess { anActor:QuerkiActor with ExtractAppProgress =>
  
  lazy val SpacePersistence = interface[querki.spaces.SpacePersistence]
  
  def extractApp(elements:Seq[OID])(implicit state:SpaceState):Future[SpaceInfo] = {
    backupSpace()
    
    
    
    // TODO -- return the info about the App
    ???
  }
  
  import querki.db._

  /**
   * Before we start, do a backup to be on the safe side.
   * 
   * TODO: in principle, this should be extracted out to another Actor. In practice, it can
   * go away once we move to Akka Persistence anyway, so we're not going to worry about it too much.
   */
  def backupSpace()(implicit state:SpaceState) = {
    QDB(ShardKind.User) { implicit conn =>
      SpacePersistence.SpaceSQL(state.id, """
          CREATE TABLE {bname} LIKE {tname}
          """).executeUpdate
      SpacePersistence.SpaceSQL(state.id, """
          INSERT {bname} SELECT * FROM {tname}
          """).executeUpdate      
    }
  }
}