package querki.types.impl

import models.SimplePTypeBuilder
import models.{FormFieldInfo, IndexedOID, Kind, OID, PropertyBundle, PType, UnknownOID, Wikitext}
import models.Thing.emptyProps

import querki.core.MOIDs.InternalPropOID
import querki.ecology._

import querki.values._

import querki.types._

class TypesModule(e:Ecology) extends QuerkiEcot(e) with Types with ModelTypeDefiner with querki.core.MethodDefs {
  import MOIDs._
    
  /******************************************
   * TYPES
   ******************************************/
  
  /**
   * This is a special Type, still somewhat half-baked, which can be wrapped around *any* QValue.
   * For the moment, I haven't thought through all the issues of UI and persistence, so it is just
   * for internal use, for the DefaultProp, but it's a start.
   */
  class WrappedValueType(tid:OID) extends SystemType[QValue](tid,
      toProps(
        setName("Wrapped Value Type"),
        (InternalPropOID -> ExactlyOne(YesNoType(true)))
      )) with SimplePTypeBuilder[QValue]
  {
    def doDeserialize(v:String)(implicit state:SpaceState) = { throw new Exception("WrappedValueType does not implement doDeserialize") }
    def doSerialize(v:QValue)(implicit state:SpaceState) = { throw new Exception("WrappedValueType does not implement doSerialize") }
    
    def doWikify(context:QLContext)(v:QValue, displayOpt:Option[Wikitext] = None) = { throw new Exception("WrappedValueType does not implement doWikify") }
    
    def doDefault(implicit state:SpaceState) = { throw new Exception("WrappedValueType does not implement doDefault") }
  }
  lazy val WrappedValueType = new WrappedValueType(WrappedValueTypeOID)
  
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
  
  override lazy val types = Seq(
    WrappedValueType
  )
  
  /***********************************************
   * API
   ***********************************************/  
  
  private def getModelType(prop:Property[_,_]):Option[ModelTypeDefiner#ModelType] = {
    val pt = prop.pType
    pt match {
      case mt:ModelTypeDefiner#ModelType => Some(mt)
      case _ => None
    }    
  }
  
  /**
   * If there is an existing Thing we are working, and it has the specified Model Property, fetch its value.
   */
  private def getChildBundle(existingOpt:Option[PropertyBundle], mt:ModelTypeDefiner#ModelType, modelProp:Property[ModeledPropertyBundle,_], indexOpt:Option[Int])(implicit state:SpaceState):Option[PropertyBundle] = {
    for {
      existing <- existingOpt
      childPV <- existing.getPropOpt(modelProp)
      childQV = childPV.v
      childBundle <- indexOpt.flatMap(index => childQV.nthAs(index, mt)).orElse(childQV.firstAs(mt))
    }
      yield childBundle
  }
  
  /**
   * If there is an existing value, splice the new childBundle into it; otherwise, just create the QValue.
   */
  private def replaceChildBundle(existingOpt:Option[PropertyBundle], childBundle:ElemValue, modelProp:Property[ModeledPropertyBundle,_], indexOpt:Option[Int])(implicit state:SpaceState):QValue = {
    val result = for {
      existing <- existingOpt
      childPV <- existing.getPropOpt(modelProp)
      index <- indexOpt
      childQV = childPV.v
      newQV = childQV.replaceNth(index, childBundle)
    }
      yield newQV
      
    result.getOrElse(ExactlyOne(childBundle))
  }
  
  /**
   * This expects that containers contains all the props *except* the innermost, real child property.
   */
  def rebuildBundle(existingOpt:Option[PropertyBundle], containers:List[IndexedOID], innerV:FormFieldInfo)(implicit state:SpaceState):Option[FormFieldInfo] = {
    val ret = containers match {
      case propIId :: rest => {
        val propId = propIId.id
        val index = propIId.i
        for {
          prop <- state.prop(propId)
          mt <- getModelType(prop)
          modelProp <- prop.confirmType(mt)
          // If we have an existing bundle value with the appropriate index, we're overriding that:
          childBundleOpt = getChildBundle(existingOpt, mt, modelProp, index)
          rebuiltChild <- rebuildBundle(childBundleOpt, rest, innerV)
          childVal <- rebuiltChild.value
          childPropId = rebuiltChild.propId
          oldProps = childBundleOpt.map(_.props).getOrElse(emptyProps)
          newProps = oldProps + (childPropId -> childVal)
          newVal = replaceChildBundle(existingOpt, mt(SimplePropertyBundle(newProps)), modelProp, index)
        }
          yield FormFieldInfo(modelProp, Some(newVal), false, true)
      }
      case Nil => Some(innerV)
    }
    ret
  }
  
  /***********************************************
   * PROPERTIES
   ***********************************************/
  
  lazy val ModelForTypeProp = new SystemProperty(ModelForTypePropOID, LinkType, ExactlyOne,
    toProps(
      setName("_modelForType"),
      AppliesToKindProp(Kind.Type),
      Summary("This receives a Model Type, and produces the Model that it is based upon."),
      SkillLevel(SkillLevelAdvanced)))

  lazy val MinTextLengthProp = new SystemProperty(MinTextLengthOID, IntType, ExactlyOne,
    toProps(
      setName("Minimum Text Length"),
      AppliesToKindProp(Kind.Property),
      Summary("The minimum length allowed in this Text, Large Text or PlainText Property"),
      Details("""If you add this meta-Property to your Text Property, it defines
          |the minimum length that will be accepted in user-entered text.
          |
          |The Text value will be trimmed of leading and trailing whitespace before this test,
          |so a Text composed entirely of spaces is still considered to be length 0. (Since for
          |most output purposes, leading and trailing spaces don't exist.)""".stripMargin)))
  
  lazy val MinIntValueProp = new SystemProperty(MinIntValueOID, IntType, ExactlyOne,
    toProps(
      setName("Minimum Number Value"),
      AppliesToKindProp(Kind.Property),
      Summary("The minimum value allowed in this Whole Number Property")))
  
  lazy val MaxIntValueProp = new SystemProperty(MaxIntValueOID, IntType, ExactlyOne,
    toProps(
      setName("Maximum Number Value"),
      AppliesToKindProp(Kind.Property),
      Summary("The maximum value allowed in this Whole Number Property")))
  
  lazy val DefaultValueProp = new SystemProperty(DefaultValuePropOID, WrappedValueType, ExactlyOne,
    toProps(
      setName("Default Value"),
      Core.InternalProp(true),
      // Strictly speaking, it's not obvious that this is non-inherited. But for the moment, the usage
      // of this property (in Property.default) can't look up the Model chain, so it is effectively
      // uninherited. We'll see if we are motivated to change that. (Keep in mind that inherited Properties
      // are, so far, unknown.)
      Core.NotInheritedProp(true),
      AppliesToKindProp(Kind.Property)))
  
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
      bundle.props.map(pair => ExactlyOne(PropAndValType(pair)))
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
    ModelForTypeProp,
    
    MinTextLengthProp,
    
    MinIntValueProp,
    MaxIntValueProp,
    
    DefaultValueProp,
    
    foreachPropertyMethod,
    valMethod,
    propMethod
  )
}
