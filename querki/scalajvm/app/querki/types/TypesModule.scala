package querki.types.impl

import scala.concurrent.Future

import models.SimplePTypeBuilder
import models.{FormFieldInfo, IndexedOID, Kind, OID, PropertyBundle, PType, UnknownOID, Wikitext}
import models.Thing.emptyProps

import querki.core.MOIDs.InternalPropOID
import querki.ecology._
import querki.util.QLog
import querki.values._

import querki.types._

class TypesModule(e:Ecology) extends QuerkiEcot(e) with Types with ModelTypeDefiner with EcologyMember {
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
        (InternalPropOID -> ExactlyOne(YesNoType(true))),
        Summary("A wrapper that can contain any Value"),
        Details("""There are a few cases where you would really like a Property to be able to take
            |*any* value, not just a single Type. The Wrapped Value Type exists for those cases.
            |
            |This is *extremely* half-baked at this time, and is only used as a way for system Properties
            |to declare a Default Value. It would be really nice for user Properties to also be able to
            |declare a Default Value, but this is pretty hard: there are complications around both the
            |user interface and persistence. So for the time being, Wrapped Value Type and Default Value
            |are for system use only.""".stripMargin)
      )) with SimplePTypeBuilder[QValue]
  {
    def doDeserialize(v:String)(implicit state:SpaceState) = { throw new Exception("WrappedValueType does not implement doDeserialize") }
    def doSerialize(v:QValue)(implicit state:SpaceState) = { throw new Exception("WrappedValueType does not implement doSerialize") }
    
    def doWikify(context:QLContext)(v:QValue, displayOpt:Option[Wikitext] = None, lexicalThing:Option[PropertyBundle] = None) = 
      v.wikify(context, displayOpt, lexicalThing)
    
    def doDefault(implicit state:SpaceState) = { throw new Exception("WrappedValueType does not implement doDefault") }
    
    def doComputeMemSize(v:QValue):Int = v.memsize
  }
  lazy val WrappedValueType = new WrappedValueType(WrappedValueTypeOID)
  
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
      
    // Note: this originally said ExactlyOne, which was Very Very Bad: it's important that we produce the same Collection
    // as the Property, or we can wind up serializing the value incorrectly.
    result.getOrElse(modelProp.cType(childBundle))
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
      Categories(querki.core.CoreTag),
      Summary("This receives a Model Type, and produces the Model that it is based upon."),
      SkillLevel(SkillLevelAdvanced)))

  lazy val MinTextLengthProp = new SystemProperty(MinTextLengthOID, IntType, ExactlyOne,
    toProps(
      setName("Minimum Text Length"),
      AppliesToKindProp(Kind.Property),
      AppliesToTypesProp(TextType, LargeTextType),
      Categories(querki.editing.EditingTag),
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
      AppliesToTypesProp(IntType),
      Categories(querki.editing.EditingTag),
      Summary("The minimum value allowed in this Whole Number Property")))
  
  lazy val MaxIntValueProp = new SystemProperty(MaxIntValueOID, IntType, ExactlyOne,
    toProps(
      setName("Maximum Number Value"),
      AppliesToKindProp(Kind.Property),
      AppliesToTypesProp(IntType),
      Categories(querki.editing.EditingTag),
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
  
  lazy val AppliesToTypesProp = new SystemProperty(AppliesToTypesPropOID, LinkType, QSet,
    toProps(
      setName("Applies to Types"),
      AppliesToKindProp(Kind.Property),
      SkillLevel(SkillLevelAdvanced),
      Categories(querki.datamodel.DataModelTag),
      Summary("Says which Types this meta-Property is relevant to"),
      Details("""A "meta-Property" is a Property that you put on other Properties, to describe their behavior.
          |For example, Maximum Number Value is a meta-Property that you put on a Whole Number Property,
          |giving the largest value allowed in that Whole Number.
          |
          |Meta-Properties should usually specify Applies to Types, to describe which Types they are relevant
          |on. When you create a Property of one of those Types, all Meta-Properties that point to it will
          |be shown in the Property Editor.
          |
          |This is an extremely advanced meta-Property, mostly used internally, but it may be more useful
          |for user code in the future.""".stripMargin)))
  
  /***********************************************
   * FUNCTIONS
   ***********************************************/
  
  // WTF? This is a private sub-Ecot (one of the secret little tricks of the Ecology system), which we are
  // breaking out of TypesModule itself so that TypesModule doesn't have to depend on MethodDefs. (Which
  // causes a dependency loop with Signature.)
  object TypeFunctions extends QuerkiEcot(e) with querki.core.MethodDefs {
  
    // TODO: does this actually work? _withProperty's documentation isn't showing up, which is odd:
    lazy val WithPropertyFunction = new InternalMethod(WithPropertyOID, 
      toProps(
        setName("_withProperty"),
        SkillLevel(SkillLevelAdvanced),
        Categories(querki.datamodel.DataModelTag),
        Summary("Add a Property to a Model Value"),
        Details("""```
            |MODEL VALUE -> PROP._withProperty(VALUE) -> MODEL VALUE
            |```
            |This allows you to add another Property to a complex Value as you are working with it.
            |This is just for rendering -- the added value does not get saved anywhere.
            |
            |This function is quite advanced, and doesn't do much hand-holding yet.""".stripMargin)))
    {
      override def qlApply(inv:Invocation):QFut = {
        for {
          prop <- inv.definingContextAsProperty
          v <- inv.processParam(0)
          bundle <- inv.contextAllBundles
          if (!bundle.isThing)
          modeled = bundle.asInstanceOf[ModeledPropertyBundle]
        }
          yield ExactlyOne(modeled.modelType(SimplePropertyBundle(modeled.props + (prop.id -> v))))
      }
    }
    
    override lazy val props = Seq(WithPropertyFunction)
  }

  override lazy val props = Seq(
    ModelForTypeProp,
    
    MinTextLengthProp,
    
    MinIntValueProp,
    MaxIntValueProp,
    
    DefaultValueProp,
    AppliesToTypesProp
  )
}
