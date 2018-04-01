package models

import querki.ecology._
import querki.globals._
import querki.ql.Invocation
import querki.values.{ElemValue, EmptyValue, QFut, QLContext}

trait Models extends EcologyInterface {
  def PropValType:PType[PropMap] with SimplePTypeBuilder[PropMap]
}

object ModelMOIDs extends EcotIds(64) {
  val PropValTypeOID = moid(1)
}

/**
 * The Ecot for the Models. This is mainly responsible for dealing with serialization.
 */
class ModelEcot(e:Ecology) extends QuerkiEcot(e) with Models {
  import ModelPersistence._
  import ModelMOIDs._
  
  override def persistentMessages = persist(64,
    (classOf[DHPropMap] -> 100),
    (classOf[DHThingState] -> 101),
    (classOf[DHProperty] -> 102),
    (classOf[DHModelType] -> 103),
    (classOf[DHSpaceState] -> 104)
  )

  // TODO: this is now only being in SpaceEcot, internally in _createThing and _changeProperties. It should arguably
  // get moved there, and maybe could be removed entirely.
  lazy val PropValType = new SystemType[PropMap](PropValTypeOID, 
    toProps(
      setName("_propValType"),
      setInternal,
      Summary("Encapsulates a single Property Value."),
      Details("This is simply a standard PropMap, with only a single value -- essentially a degenerate PropertyBundle.")
    )) with SimplePTypeBuilder[PropMap]
  {
    def doDeserialize(ser:String)(implicit state:SpaceState):PropMap = ???
    def doSerialize(v:PropMap)(implicit state:SpaceState):String = ???
    def doWikify(context:QLContext)(v:PropMap, displayOpt:Option[Wikitext] = None, lexicalThing:Option[PropertyBundle] = None):Future[Wikitext] = {
      v.values.headOption.map(_.wikify(context, displayOpt, lexicalThing)).getOrElse(fut(Wikitext.empty))
    }
    def doDefault(implicit state:SpaceState):PropMap = ???
    def doComputeMemSize(v:PropMap):Int = 0
  }

  override lazy val types = Seq(
    PropValType
  )
}
