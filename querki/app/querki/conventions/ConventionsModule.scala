package querki.conventions

import models.Kind
import models.Thing._

import models.system.{ExactlyOne, Optional}
import models.system.{LargeTextType, TextType, YesNoType}
import models.system.{SystemProperty, AppliesToKindProp}
import models.system.OIDs.{sysId, NotInheritedOID}

import modules.Module

/**
 * This Module defines common Querki "conventions" -- things that aren't terribly *important*
 * to the operation of the system, but are common Properties and such that are useful enough
 * to encourage their general use.
 */
class ConventionsModule(val moduleId:Short) extends Module {
  object MOIDs {
    // Old OIDs, moved to here:
    val PropSummaryOID = sysId(85)
    val PropDetailsOID = sysId(86)
    
    val PropDescriptionOID = moid(1)
  }
  import MOIDs._

  lazy val PropSummary = new SystemProperty(PropSummaryOID, TextType, Optional,
    toProps(
      setName("Summary"),
      (PropSummaryOID -> Optional(TextType("This is an optional one-line description of something."))),
      (PropDetailsOID -> Optional(LargeTextType("""When you define a Property, you may add this Summary as
          |part of that definition. It will be displayed in mouseover hovering and things like that, to help
          |you and others remember what this Property is. It's optional, but recommended that you define
          |this for all of your Properties.
          |
          |You may also put Summary on whatever other Things you like, and use it from other Things. By
          |default (if you use Summary and it isn't defined), it simply shows this Thing's Display Name.
          |
          |There is no required format or content for this Summary -- it should simply be a reminder of what
          |this Thing is about.""".stripMargin)))))
// TODO: for reasons I totally don't understand, this override completely fails. So okay -- we should probably
// do this more correctly anyway, using a new meta-property for DefaultValue instead. But that is tricky, since
// the DefaultValue's Collection and Type should match those of the Property it is associated with...
//  {
//    override def default = { println("!!!! In PropSummary.default!"); ExactlyOne(TextType("""[[Display Name]]""")) }
//  }

  lazy val PropDetails = new SystemProperty(PropDetailsOID, LargeTextType, Optional,
    toProps(
      setName("Details"),
      (PropSummaryOID -> Optional(TextType("This is an optional detailed description of something."))),
      (PropDetailsOID -> Optional(LargeTextType("""When you define a Property, you may add whatever description
          |or documentation you see fit in the Details. This is the place to say what this Property is for, what
          |it means, how it should be used, what is legal in it, and so on.
          |
    	  |It is entirely optional -- put something in here if it makes sense. In general, the more complex the
          |Space, and the more people who will be using it, the wiser it becomes to give Details for all of your
          |Properties. If this Space is simple and just for you, it usually isn't necessary.""".stripMargin)))))

  lazy val PropDescription = new SystemProperty(PropDescriptionOID, LargeTextType, ExactlyOne,
    toProps(
      setName("Description"),
      (PropSummaryOID -> Optional(TextType("This is the full description of something."))),
      (PropDetailsOID -> Optional(LargeTextType("""This Property lets you describe something, in as much detail
          |as seems appropriate.
          |
          |In general, you should probably use Description *or* Summary plus Details, depending on what makes most
          |sense for your case. By default, Description will show the Summary, followed by the Details.""".stripMargin)))))
// TODO: see Summary. Need to find a functioning way to define a Default value.
//  {
//    override def default = ExactlyOne(LargeTextType("""[[Summary]] [[Details -> ""
//        |
//        |____""]]""".stripMargin))    
//  }
  
  override lazy val props = Seq(
    PropSummary,
    PropDetails,
    PropDescription
  )
}