package querki.logic

import models.{OID, ThingState}
import models.Thing._
import models.system.{ExactlyOne}
import models.system.{YesNoType}
import models.system.{DisplayTextProp}
import models.system.OIDs.{RootOID, systemOID}

import ql.QLPhrase

import querki.ecology._
import querki.values._

import modules.Module

object MOIDs extends EcotIds(9) {
  val TrueOID = moid(1)
  val FalseOID = moid(2)
}

/**
 * This module should eventually contain everything pertaining to predicate logic.
 * 
 * For now, most of the stuff that should be here is still in the core System Space,
 * but that should be fixed. This should absorb YesNoType, and all of the predicate-oriented
 * methods.
 */
class LogicModule(e:Ecology) extends Module(e) {
  import MOIDs._
  
  /******************************************
   * THINGS
   ******************************************/
  
  class BooleanValue(tid:OID, elem:ElemValue, pf:PropFetcher) extends ThingState(tid, systemOID, RootOID, pf)
  {
    val v = ExactlyOne(elem)
    override def qlApply(context:QLContext, params:Option[Seq[QLPhrase]] = None):QValue = v
  }
  
  lazy val trueVal = new BooleanValue(TrueOID, YesNoType.True,
      toProps(
        setName("True"),
        DisplayTextProp("""The literal true value, for use in QL expressions.""")))
  
  lazy val falseVal = new BooleanValue(FalseOID, YesNoType.False,
      toProps(
        setName("False"),
        DisplayTextProp("""The literal false value, for use in QL expressions.""")))

  override lazy val things = Seq(
    trueVal,
    falseVal
  )
}