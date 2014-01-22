package querki

import querki.ecology._

import models.Property

package object datamodel {

	object MOIDs extends EcotIds(21) {
	  val InstancesMethodOID = sysId(44)
	  val RefsMethodOID = sysId(48)
	  val SpaceMethodOID = sysId(59)
	  val ExternalRootsOID = sysId(60)
	  val ChildrenMethodOID = sysId(62)
	  val IsModelMethodOID = sysId(63)
	  val PropsOfTypeOID = sysId(76)
	  val IsDefinedOID = sysId(78)
	  val AllPropsMethodOID = sysId(83)
	  val OIDMethodOID = sysId(90)
	  val KindMethodOID = sysId(91)
	  val CurrentSpaceMethodOID = sysId(92)
	  val IsMethodOID = sysId(93)
	  val IsFunctionOID = sysId(104)
	  
	  val HasPropertyMethodOID = moid(1)
	  val AllThingsOID = moid(2)
	}

  trait DataModelAccess extends EcologyInterface {
    def IsFunctionProp:Property[Boolean,Boolean]
  }
}