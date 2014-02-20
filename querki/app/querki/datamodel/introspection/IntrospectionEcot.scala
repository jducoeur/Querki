package querki.datamodel.introspection

import models.{DisplayPropVal, PropertyBundle, SimplePTypeBuilder, UnknownOID, Wikitext}

import querki.ecology._
import querki.util.PublicException
import querki.values.{QLContext, SpaceState}

object MOIDs extends EcotIds(34) {
  val ForeachPropertyMethodOID = moid(1)
  val ValMethodOID = moid(2)
  val PropMethodOID = moid(3)  
  val DefinedOnMethodOID = moid(4)
  val IsInheritedMethodOID = moid(5)
}

class IntrospectionEcot(e:Ecology) extends QuerkiEcot(e) with querki.core.MethodDefs {
  import MOIDs._
  
  lazy val PropListMgr = interface[querki.core.PropListManager]
  lazy val QL = interface[querki.ql.QL]
      
  /******************************************
   * TYPES
   ******************************************/
  
  /**
   * This Type is intentionally purely internal for the time being -- we don't allow users to instantiate it, or even see it much.
   */
  lazy val PropAndValType = new SystemType[DisplayPropVal](UnknownOID,
      toProps(
        setName("Prop and Val Type"),
        Core.InternalProp(true))) with SimplePTypeBuilder[DisplayPropVal]
  {
    def doDeserialize(v:String)(implicit state:SpaceState) = ???
    def doSerialize(v:DisplayPropVal)(implicit state:SpaceState) = ???
    
    def doWikify(context:QLContext)(v:DisplayPropVal, displayOpt:Option[Wikitext] = None) = { 
      val propName = v.prop.displayName
      val vDisplay = v.v match {
        case Some(v) => v.wikify(context, displayOpt) 
        case None => Wikitext.empty
      }
      Wikitext(s"$propName: $vDisplay")
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
            ExactlyOne(PropAndValType(displayVal))
          }
        }
        
        case None => bundle.props.map{ pair =>
          val propOpt = state.prop(pair._1)
          propOpt match {
            case Some(prop) => ExactlyOne(PropAndValType(DisplayPropVal(Some(bundle), prop, Some(pair._2))))
            case None => throw new PublicException("Func.unknownPropOID", pair._1)
          }
        }
      }
    }
    
    override def qlApply(invIn:Invocation):QValue = {
      val inv = invIn.preferDefiningContext
      implicit val state = inv.state
      for {
        bundle <- inv.contextAllBundles()
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
        dpv <- inv.contextAllAs(PropAndValType)
      }
        yield dpv.effectiveV match {
          case Some(v) => v
          case None => Core.QNone
        }
    }
  }
  
  lazy val propMethod = new InternalMethod(PropMethodOID,
      toProps(
        setName("_prop"),
        Summary("Fetch the current Property, inside of _foreachProperty")))
  {
    override def qlApply(inv:Invocation):QValue = {
      for {
        dpv <- inv.contextAllAs(PropAndValType)
      }
        yield ExactlyOne(LinkType(dpv.prop))
    }
  }
  
  lazy val definedOnMethod = new InternalMethod(DefinedOnMethodOID,
      toProps(
        setName("_definedOn"),
        Summary("Fetch the Thing or Model Value that the current Property was defined on, inside of _foreachProperty")))
  {
    override def qlApply(inv:Invocation):QValue = {
      for {
        dpv <- inv.contextAllAs(PropAndValType)
      }
        yield dpv.on match {
          case Some(on) => on match {
            case t:Thing => ExactlyOne(LinkType(t))
            // Ick: how can we make this case less ugly? We have the PropertyBundle, and need to turn it back into a QValue:
            case bundle @ querki.types.ModeledPropertyBundle(mt, _, _) => ExactlyOne(mt(querki.types.SimplePropertyBundle(bundle.props.toSeq:_*)))
          }
          case None => Core.QNone
        }
    }
  }
  
  lazy val isInheritedMethod = new InternalMethod(IsInheritedMethodOID,
      toProps(
        setName("_isInherited"),
        Summary("Says whether the current Property's value is locally defined or was inherited from the Model, inside of _foreachProperty")))
  {
    override def qlApply(inv:Invocation):QValue = {
      for {
        dpv <- inv.contextAllAs(PropAndValType)
      }
        yield ExactlyOne(YesNoType(dpv.isInherited))
    }
  }

  override lazy val props = Seq(
    foreachPropertyMethod,
    valMethod,
    propMethod,
    definedOnMethod,
    isInheritedMethod
  )
}