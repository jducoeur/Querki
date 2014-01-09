package models.system

import play.api.Logger

import models._

import Property._
import Thing._
import YesNoType._

import OIDs._
import SystemSpace._

import querki.core

import querki.ecology._
import querki.types._
import querki.values._

// TODO: replace this with the version in QuerkiEcot:
class SystemProperty[VT, -RT](pid:OID, t:PType[VT] with PTypeBuilder[VT, RT], c:Collection, p:PropFetcher)(implicit e:Ecology = querki.ecology.Ecology) 
  extends Property[VT, RT](pid, systemOID, core.MOIDs.UrPropOID, t, c, p, querki.time.epoch)(e)
  
// TEMP: while we're getting things working:
object Summary {
  def apply(text:String) = (querki.conventions.MOIDs.PropSummaryOID -> ExactlyOne(TextType(text))) 
}
object Details {
  def apply(text:String) = (querki.conventions.MOIDs.PropDetailsOID -> ExactlyOne(LargeTextType(text)))
}
object NotInherited {
  def apply() = (querki.core.MOIDs.NotInheritedOID -> ExactlyOne(YesNoType(true)))
}
object SkillLevel {
  val Basic = querki.identity.skilllevel.MOIDs.SkillLevelBasicOID
  val Standard = querki.identity.skilllevel.MOIDs.SkillLevelStandardOID
  val Advanced = querki.identity.skilllevel.MOIDs.SkillLevelAdvancedOID
  def apply(level:OID) = (querki.identity.skilllevel.MOIDs.SkillLevelPropOID -> ExactlyOne(LinkType(level)))
}
object AppliesToKindProp {
  def apply(kind:Kind.Kind) = (querki.core.MOIDs.AppliesToKindOID -> ExactlyOne(IntType(kind)))
}
object InternalProp {
  def apply(b:Boolean) = (querki.core.MOIDs.InternalPropOID -> ExactlyOne(YesNoType(b)))
}
