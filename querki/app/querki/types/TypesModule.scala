package querki.types.impl

import models.SimplePTypeBuilder
import models.{Kind, OID, Wikitext}
import models.Thing._

import models.system.{IntType, SystemType, YesNoType}
import models.system.ExactlyOne
import models.system.{APIProperty, AppliesToKindProp, InternalProp, NotInheritedProp, SystemProperty}
import models.system.OIDs.{sysId, InternalPropOID, NotInheritedOID}

import querki.conventions.{PropDetails, PropSummary}
import querki.ecology._

import querki.values._

import modules.Module

import querki.types._

class TypesModule(e:Ecology, val moduleId:Short) extends Module(e) with Types {
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
    def doDeserialize(v:String) = { throw new Exception("WrappedValueType does not implement doDeserialize") }
    def doSerialize(v:QValue) = { throw new Exception("WrappedValueType does not implement doSerialize") }
    
    def doWikify(context:QLContext)(v:QValue, displayOpt:Option[Wikitext] = None) = { throw new Exception("WrappedValueType does not implement doWikify") }
    
    def doDefault = { throw new Exception("WrappedValueType does not implement doDefault") }
  }
  lazy val WrappedValueType = new WrappedValueType(WrappedValueTypeOID)
  
  override lazy val types = Seq(
    WrappedValueType
  )
   
  /***********************************************
   * PROPERTIES
   ***********************************************/

  lazy val MinTextLengthProp = new APIProperty(querki.types.MinTextLengthProp, MinTextLengthOID, IntType, ExactlyOne,
    toProps(
      setName("Minimum Text Length"),
      AppliesToKindProp(Kind.Property),
      PropSummary("The minimum length allowed in this Text, Large Text or PlainText Property"),
      PropDetails("""If you add this meta-Property to your Text Property, it defines
          |the minimum length that will be accepted in user-entered text.
          |
          |The Text value will be trimmed of leading and trailing whitespace before this test,
          |so a Text composed entirely of spaces is still considered to be length 0. (Since for
          |most output purposes, leading and trailing spaces don't exist.)""".stripMargin)))
  
  lazy val MinIntValueProp = new APIProperty(querki.types.MinIntValueProp, MinIntValueOID, IntType, ExactlyOne,
    toProps(
      setName("Minimum Number Value"),
      AppliesToKindProp(Kind.Property),
      PropSummary("The minimum value allowed in this Whole Number Property")))
  
  lazy val MaxIntValueProp = new APIProperty(querki.types.MaxIntValueProp, MaxIntValueOID, IntType, ExactlyOne,
    toProps(
      setName("Maximum Number Value"),
      AppliesToKindProp(Kind.Property),
      PropSummary("The maximum value allowed in this Whole Number Property")))
  
  /**
   * The DefaultValueProp is pretty much what it says: it is the default value for this Property. That
   * is, it is what you get when you hard-query for a Property on a Thing (not an Opt query), and that
   * Property is *not* defined on the Thing.
   * 
   * Note that DefaultValueProp is a full-on QValue. That's because its Type is, in principle, unknown.
   * It should match the Type and Collection of the Property it is being applied to, though, if you
   * want to avoid strange and confusing behaviour.
   * 
   * In principle, it would be nice to expose this to end users to use. In practice, that's going to
   * be challenging from a UI perspective: you have to feed in the expected Type and Collection to produce
   * the correct input control. It's doable in principle, but enough work that I'm not going to bother
   * until we care.
   */
  lazy val DefaultValueProp = new APIProperty(querki.types.DefaultValueProp, DefaultValuePropOID, WrappedValueType, ExactlyOne,
    toProps(
      setName("Default Value"),
      (InternalPropOID -> ExactlyOne(YesNoType(true))),
      // Strictly speaking, it's not obvious that this is non-inherited. But for the moment, the usage
      // of this property (in Property.default) can't look up the Model chain, so it is effectively
      // uninherited. We'll see if we are motivated to change that. (Keep in mind that inherited Properties
      // are, so far, unknown.)
      (NotInheritedOID -> ExactlyOne(YesNoType(true))),
      AppliesToKindProp(Kind.Property)))

  override lazy val props = Seq(
    MinTextLengthProp,
    
    MinIntValueProp,
    MaxIntValueProp,
    
    DefaultValueProp
  )
}

object TypesModule {
}