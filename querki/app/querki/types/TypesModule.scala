package querki.types.impl

import models.SimplePTypeBuilder
import models.{FormFieldInfo, Kind, OID, PropertyBundle, Wikitext}
import models.Thing.emptyProps

import querki.core.MOIDs.InternalPropOID
import querki.ecology._

import querki.values._

import querki.types._

class TypesModule(e:Ecology) extends QuerkiEcot(e) with Types with ModelTypeDefiner {
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
  
  override lazy val types = Seq(
    WrappedValueType
  )
  
  
  
  def getModelType(prop:Property[_,_]):Option[ModelTypeDefiner#ModelType] = {
    val pt = prop.pType
    pt match {
      case mt:ModelTypeDefiner#ModelType => Some(mt)
      case _ => None
    }    
  }
  
  /**
   * This expects that containers contains all the props *except* the innermost, real child property.
   */
  def rebuildBundle(existingOpt:Option[PropertyBundle], containers:List[OID], innerV:FormFieldInfo)(implicit state:SpaceState):Option[FormFieldInfo] = {
    println("======")
    println("Existing: " + existingOpt)
    println("Containers: " + containers)
    val ret = containers match {
      case propId :: rest => {
        for {
          prop <- state.prop(propId)
          mt <- getModelType(prop)
          modelProp <- prop.confirmType(mt)
          // TODO: this is a clear bad smell -- this shouldn't be firstOpt. Do we need to build indexes into the
          // container IDs? I sadly suspect we do...
          childBundleOpt = existingOpt.flatMap(existing => modelProp.firstOpt(existing.props))
          rebuiltChild <- rebuildBundle(childBundleOpt, rest, innerV)
          childVal <- rebuiltChild.value
          childPropId = rebuiltChild.propId
          oldProps = childBundleOpt.map(_.props).getOrElse(emptyProps)
          dummy1 = println("oldProps = " + oldProps)
          newProps = oldProps + (childPropId -> childVal)
          dummy2 = println("newProps = " + newProps)
          newVal = ExactlyOne(mt(SimplePropertyBundle(newProps)))
        }
          yield FormFieldInfo(modelProp, Some(newVal), false, true)
      }
      case Nil => Some(innerV)
    }
    println("Returning: " + ret)
    println("++++++")
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

  override lazy val props = Seq(
    ModelForTypeProp,
    
    MinTextLengthProp,
    
    MinIntValueProp,
    MaxIntValueProp,
    
    DefaultValueProp
  )
}

object TypesModule {
}