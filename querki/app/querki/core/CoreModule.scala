package querki.core

import models._
import models.Thing._

import models.system.OIDs.{systemOID, DisplayTextOID, InternalPropOID}
import models.system.SystemProperty
import models.system.{UrThing}
import models.system.{LargeTextType, TextType, YesNoType}

import querki.conventions
import querki.ecology._

import modules.Module

class CoreModule(e:Ecology, val moduleId:Short) extends Module(e) with Core {
  import MOIDs._

  /***********************************************
   * PROPERTIES
   ***********************************************/

  /**
   * The root Property, from which all others derive.
   */
  lazy val UrProp = Property(UrPropOID, systemOID, UrThing, TextType, ExactlyOne,
      toProps(
        setName("Property"),
        (InternalPropOID -> ExactlyOne(YesNoType(true))),
        (conventions.MOIDs.PropSummaryOID -> Optional(TextType("The root Property, from which all others derive."))),
        (DisplayTextOID -> Optional(LargeTextType("""[[Summary -> ""**____** -- ""]]
            |[[_if(Property Type -> _is(Internal Method Type), 
            |  ""**METHOD**"",
            |  ""Collection: [[Property Collection]] Type: [[Property Type]]"")]]
            |
            |
            |[[Details]]""".stripMargin)))
        ), querki.time.epoch)

  lazy val ApplyMethod = new SystemProperty(ApplyMethodOID, QLType, Optional,
    toProps(
      setName("_apply"),
      SkillLevel(SkillLevel.Advanced),
      Summary("A QL Expression that will be run when you name this Thing."),
      Details("""_apply is an advanced function, and most users will not use it directly. But it is probably
          |the most important Property in Querki, and advanced users may want to play with it.
          |
          |One of Querki's design goals was that it should Just Work. This is reflected, more than anywhere else,
          |in the fact that you can just say:
          |[[_code(""[[My Thing]]"")]]
          |and it shows up as a pointer to *My Thing*.
          |
          |That seems obvious, but consider -- you can also say:
          |[[_code(""[[My Property]]"")]]
          |and what you get isn't a pointer to *My Property* -- instead, you get the *value* of My Property on the
          |Thing you're looking at.
          |
          |Moreover, you can say:
          |[[_code(""[[All Things]]"")]]
          |on a page, and what you get is a listing of all of the Things in this Space! So what the heck is going on
          |here?
          |
          |The secret behind the magic is the _apply method. _apply is a Property that is defined on *every* Thing.
          |(More or less -- system-defined Things use a closely-related built-in mechanism.) It defines exactly
          |"What should happen when I name this Thing?" So _apply on Properties displays the value of the Property
          |on the received Thing; _apply on All Things is this QL Expression:
          |[[_code(All Things._apply)]]
          |And _apply for Thing (the Model that everything is based on) simply produces a pointer to this thing.
          |
          |You can define _apply for your own Things as well -- indeed, the way you usually write your own serious
          |Methods is to define a Thing that just has an _apply Property, and then you can use the Method just like
          |the system-defined ones, by name.
          |
          |The QL Expression in the _apply Property will receive whatever is passed in, and should produce whatever
          |you want to pass out. It is currently completely unstructured and untyped. However, note that we will
          |probably be moving towards more structure in the future, and you should always try to be consistent:
          |as with any QL Expression, you should expect to receive a specific Type, and always produce a specific Type.""".stripMargin)))

  lazy val NotInheritedProp = new SystemProperty(NotInheritedOID, YesNoType, ExactlyOne,
    toProps(
      setName("Not Inherited"),
      // Need to define this explicitly, to break infinite loops in lookup:
      (NotInheritedOID -> ExactlyOne(querki.values.ElemValue(false, new DelegatingType({YesNoType})))),
      AppliesToKindProp(Kind.Property),
      SkillLevel(SkillLevel.Advanced),
      Summary("Should this Property be inherited from ancestors?"),
      Details("""All Things in Querki are part of a big inheritance tree. Instances inherit from their Models,
          |which in turn usually inherit from higher-level Models.
          |
          |When we say that they "inherit", we mean that the lower-level Things (the Instances) pick up default
          |values of their Properties from their higher-level ancestors (the Models). So for instance, if the Model
          |defines a Default View, the Instance will use that Default View unless it redefines it.
          |
          |This is almost always the right way for things to work, but a very few Properties are different: they
          |must be defined on each individual Thing they apply to, and should *not* be inherited. For example, the
          |*Is a Model* flag must not be inherited, because if it was, then all the Instances of a Model would
          |themselves be marked as Models.
          |
          |So you can use the *Not Inherited* flag to indicate that this Property should not be inherited from
          |Models to Things. This is a very advanced Property, and not intended for ordinary use.""".stripMargin)
      ))

  override lazy val props = Seq(
    ApplyMethod,
    NotInheritedProp,
    UrProp
  )
}
