package querki

import models.{OID, Property, PType, PTypeBuilder}

import querki.basic.PlainText
import querki.ecology._

import querki.values.QValue

package object links {
  object PublicMOIDs extends EcotIds(27) {
    val LinkKindOID = sysId(33)
    val LinkAllowAppsOID = sysId(34)
    val LinkModelOID = sysId(35)
    val OldExternalLinkTypeOID = sysId(41)
    val LinkToModelsOnlyOID = sysId(70)
    val NoCreateThroughLinkOID = sysId(103)
  }

  // Why not java.net.URL? Because it just plain can't cope with simply relative URLs -- it always wants
  // to wind up with an absolute URL. But that's silly: we frequently want a relative URL, and specifically
  // *don't* want to be encoding the whole damned thing here.
  case class QURL(url:String) {
    val legalChars = """\w\d\-\._\~:/\?#\[\]@!$&'\(\)\*\+,;=%"""
    if (!url.matches(s"[$legalChars]*"))
      throw new Exception("Not a legal URL: " + url)
  }
  
  trait Links extends EcologyInterface {
    def OldExternalLinkType:PType[QURL] with PTypeBuilder[QURL, String]
    
    def LinkAllowAppsProp:Property[Boolean,Boolean]
    def LinkKindProp:Property[Int,Int]
    def LinkModelProp:Property[OID,OID]
    def LinkToModelsOnlyProp:Property[Boolean,Boolean]
    def NoCreateThroughLinkProp:Property[Boolean,Boolean]
    
    def LinkValue(target:OID):QValue
  }
}