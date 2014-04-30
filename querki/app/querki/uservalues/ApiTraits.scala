package querki.uservalues

import models.{OID, Property}

import querki.values.QValue

/**
 * This message is sent from the UserSession, through the Space to the UserValueSpacePlugin,
 * telling it to do create a Summary in a properly synchronized way.
 */
case class SummarizeChange[UVT](tid:OID, fromProp:Property[UVT,_], summaryId:OID, previous:Option[QValue], current:Option[QValue])
