package querki.datamodel.introspection

import models.{PropertyBundle, SimplePTypeBuilder, UnknownOID, Wikitext}

import querki.ecology._
import querki.values.{QLContext, SpaceState}

object MOIDs extends EcotIds(34) {
  val ForeachPropertyMethodOID = moid(1)
  val ValMethodOID = moid(2)
  val PropMethodOID = moid(3)  
}

class IntrospectionEcot(e:Ecology) extends QuerkiEcot(e) with querki.core.MethodDefs {
  import MOIDs._
  
  lazy val PropListMgr = interface[querki.core.PropListManager]
      
  /******************************************
   * TYPES
   ******************************************/
  
  /**
   * This Type is intentionally purely internal for the time being -- we don't allow users to instantiate it, or even see it much.
   */
  lazy val PropAndValType = new SystemType[(OID, QValue)](UnknownOID,
      toProps(
        setName("Prop and Val Type"),
        Core.InternalProp(true))) with SimplePTypeBuilder[(OID, QValue)]
  {
    def doDeserialize(v:String)(implicit state:SpaceState) = ???
    def doSerialize(v:(OID, QValue))(implicit state:SpaceState) = ???
    
    def doWikify(context:QLContext)(v:(OID, QValue), displayOpt:Option[Wikitext] = None) = { 
      val propName = context.state.prop(v._1) match {
        case Some(p) => p.displayName
        case _ => v._1.toString
      }
      Wikitext(s"$propName: ${v._2.wikify(context, displayOpt)}")
    }
    
    def doDefault(implicit state:SpaceState) = ???  
  }
  
  /***********************************************
   * FUNCTIONS
   ***********************************************/
  
  lazy val foreachPropertyMethod = new InternalMethod(ForeachPropertyMethodOID,
    toProps(
      setName("_foreachProperty"),
      Summary("Applies the given function to each Property in the received Thing or Model Value"),
      Details("""    THING or MODEL VALUE -> _foreachProperty(... code ...) -> RESULTS
          |
          |This function is appropriate if you want to do something to each Property in a given Thing or Model Value.
          |It takes all of the Properties found in there, turns each into a PropAndValue, and hands it off to the
          |code contained in the parameter. The results are then bundled back up as a collection, and produced together.
          |
          |Note that the PropAndValue passed into the code is a combination of both the link to the Property *and*
          |the Value of that Property in this Thing or Model Value. You can access those parts using the _prop and _val
          |functions.""".stripMargin)))
  {
    def bundle2Props(bundle:PropertyBundle)(implicit state:SpaceState):Iterable[QValue] = {
      bundle.getModelOpt match {
        case Some(model) => {
          val propList = PropListMgr.from(bundle)
          val orderedList = PropListMgr.prepPropList(propList, model, state)
          orderedList.filterNot(_._2.effectiveV.isEmpty).map { propListEntry =>
            val (prop, displayVal) = propListEntry
            ExactlyOne(PropAndValType((prop.id -> displayVal.effectiveV.get)))
          }
        }
        case None => bundle.props.map(pair => ExactlyOne(PropAndValType(pair)))
      }
    }
    
    override def qlApply(invIn:Invocation):QValue = {
      val inv = invIn.preferDefiningContext
      implicit val state = inv.state
      for {
        bundle <- inv.contextAllBundles
        propAndVal <- inv.iter(bundle2Props(bundle))
        result <- inv.processParam(0, inv.context.next(propAndVal))
      }
        yield result
    }
  }
  
  lazy val valMethod = new InternalMethod(ValMethodOID,
      toProps(
        setName("_val"),
        Summary("Fetch the Value part, inside of _foreachProperty")))
  {
    override def qlApply(inv:Invocation):QValue = {
      for {
        pair <- inv.contextAllAs(PropAndValType)
      }
        yield pair._2
    }
  }
  
  lazy val propMethod = new InternalMethod(PropMethodOID,
      toProps(
        setName("_prop"),
        Summary("Fetch the current Property, inside of _foreachProperty")))
  {
    override def qlApply(inv:Invocation):QValue = {
      for {
        pair <- inv.contextAllAs(PropAndValType)
      }
        yield ExactlyOne(LinkType(pair._1))
    }
  }

  override lazy val props = Seq(
    foreachPropertyMethod,
    valMethod,
    propMethod
  )
}