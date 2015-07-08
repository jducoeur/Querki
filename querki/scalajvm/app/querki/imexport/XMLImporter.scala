package querki.imexport

import querki.globals._
import querki.session.AutowireApiImpl
import querki.values.RequestContext

/**
 * This object is responsible for taking the XML representation of a Space and importing it into
 * the local Querki instance as a new Space.
 * 
 * Note that this isn't standalone -- it assumes that it is mixed in with an AutowireApiImpl,
 * and it interacts *heavily* with the SpaceManager and the new Space.
 * 
 * @author jducoeur
 */
private [imexport] class XMLImporter { self:AutowireApiImpl =>
  def createSpaceFrom(xml:String) = {
    // Read the XML in its raw form, and build a SpaceState from that. This is only the beginning
    // of the process, though -- then we need to actually import all those Things into the DB, and
    // reassign all of the OIDs.
    val rawState = new RawXMLImport(rc)(ecology).readXML(xml)
  }
}