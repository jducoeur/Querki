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

  lazy val PropValType = new SystemType[PropMap](PropValTypeOID, 
    toProps(
      setName("_propBundleType"),
      setInternal,
      Summary("Encapsulates some Property Values."),
      Details("""This is a bunch of values bundled together -- similar to a Thing, but more ad-hoc. It cannot be saved
                |or used as a normal Type, but some functions allow you to pass it and use it in QL.""".stripMargin)
    )) with SimplePTypeBuilder[PropMap]
  {
    def doDeserialize(ser:String)(implicit state:SpaceState):PropMap = ???
    def doSerialize(v:PropMap)(implicit state:SpaceState):String = ???
    def doWikify(context:QLContext)(v:PropMap, displayOpt:Option[Wikitext] = None, lexicalThing:Option[PropertyBundle] = None):Future[Wikitext] = {
      implicit val state = context.state
      val wikiFuts = v.map { case (propId, pv) =>
        val propName = state.prop(propId).map(_.displayName).getOrElse("Unknown Property!")
        pv.wikify(context, displayOpt, lexicalThing).map { wiki =>
          Wikitext(s": $propName : ") + wiki + Wikitext.nl + Wikitext.nl
        }
      }
      val futWikis = Future.sequence(wikiFuts)
      futWikis.map { wikis => wikis.reduce(_ + _)}
    }
    def doDefault(implicit state:SpaceState):PropMap = ???
    def doComputeMemSize(v:PropMap):Int = 0
  }

  override lazy val types = Seq(
    PropValType
  )
}
