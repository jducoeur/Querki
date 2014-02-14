package querki

import models.Thing

import querki.ecology._
import querki.values.{RequestContext, SpaceState}

/**
 * This package encapsulates the Import and Export operations for Querki. We bundle both together,
 * since they are often closely related. (Eventually, we expect them to be intimately related, when
 * we get into Synchronization.)
 */
package object imexport {

  /**
   * The flag indicating the format to import/export.
   */
  object Format {    
    val Unknown = 0
    val CSV = 1
  }
  type Format = Int

  /**
   * The results of an Export request, to be sent to the user.
   */
  trait ExportedContent {
    def content:Array[Byte]
    def name:String
  }
  
  trait Imexport extends EcologyInterface {
    /**
     * Export the Instances of the specified Model.
     * 
     * Will throw a PublicException if something goes wrong, so this should be wrapped in a TryTrans.
     */
    def exportInstances(rc:RequestContext, format:Format, model:Thing)(implicit state:SpaceState):ExportedContent
  }
}