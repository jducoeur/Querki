package querki

import models.{MIMEType, OID, Property, Thing}
import querki.ecology._
import querki.values.{RequestContext, SpaceState}

/**
 * This package encapsulates the Import and Export operations for Querki. We bundle both together,
 * since they are often closely related. (Eventually, we expect them to be intimately related, when
 * we get into Synchronization.)
 */
package object imexport {

  final val ImportExportCategory = "Import/Export"

  /**
   * The flag indicating the format to import/export.
   */
  object Format {
    val Unknown = 0
    val CSV = 1
    val XML = 2
  }
  type Format = Int

  /**
   * The results of an Export request, to be sent to the user.
   */
  trait ExportedContent {
    def content: Array[Byte]
    def name: String
    def mime: MIMEType.MIMEType
  }

  trait Imexport extends EcologyInterface {

    /**
     * Imported Things get this Property, saying what their original OID was in the exported Space.
     */
    def OldOIDProperty: Property[OID, OID]

    /**
     * Export the Instances of the specified Model.
     *
     * Will throw a PublicException if something goes wrong, so this should be wrapped in a TryTrans.
     */
    def exportInstances(
      rc: RequestContext,
      format: Format,
      model: Thing
    )(implicit
      state: SpaceState
    ): ExportedContent

    /**
     * Export an entire Space as XML.
     *
     * For now, we're not bothering to take a Format; I'm not sure we're ever going to do a full
     * export to anything *but* XML.
     *
     * TODO: in the medium term, this should produce a stream of bytes, not a String.
     */
    def exportSpace(rc: RequestContext)(implicit state: SpaceState): String
  }
}
