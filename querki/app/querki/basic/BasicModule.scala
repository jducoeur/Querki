package querki.basic

import models._
import models.Thing._

import models.system.OIDs.{systemOID, RootOID}
import models.system.SystemProperty
import models.system.{Optional}
import models.system.{PlainTextType}
import models.system.{DeprecatedProp, DisplayTextProp}

import querki.conventions._
import querki.core._
import querki.ecology._
import querki.types._

class BasicModule(e:Ecology) extends QuerkiEcot(e) with Basic {
  import MOIDs._
  
  val Core = initRequires[querki.core.Core]
  val DeriveName = initRequires[querki.types.DeriveName]
  val Types = initRequires[querki.types.Types]
  
  lazy val ApplyMethod = Core.ApplyMethod
  lazy val IsModelProp = Core.IsModelProp
  
  /***********************************************
   * PROPERTIES
   ***********************************************/

  /**
   * If set, this is the display name of the specified object. Whereas the primary NameProp
   * has a number of restrictions, the DisplayNameProp does not. It is used to list a Thing
   * by preference when it is set.
   */
  lazy val DisplayNameProp = new SystemProperty(DisplayNameOID, PlainTextType, Optional,
    toProps(
      setName("Display Name"),
      NotInherited,
      Types.MinTextLengthProp(1),
      Summary("How to show this Thing's Name"),
      Details("""Most Things in Querki have a Name. (It isn't strictly required, but strongly encouraged most
          |of the time.) In general, when we list a Thing, we show its Name. However, if you want to display
          |something *other* than its Name instead, set its Display Name Property to show in its place.
          |
          |Display Name is mainly useful when the name you would like to use includes characters that aren't
          |legal in Names, such as quotes, apostrophes, commas or other punctuation characters.
          |
          |Note that the relationship of Name and Display Name is still in some flux, and things may shift a
          |bit over time. We are thinking of putting Display Name more front-and-center, and making Name derive
          |from that instead.""".stripMargin)
      ))

  override lazy val props = Seq(
    DisplayNameProp
  )
  
  /***********************************************
   * THINGS
   ***********************************************/

  lazy val SimpleThing = ThingState(SimpleThingOID, systemOID, RootOID,
    toProps(
      setName("Simple-Thing"),
      IsModelProp(true),
      DisplayTextProp(Optional.QNone),
      (querki.basic.MOIDs.DisplayNameOID -> Optional.QNone),
      DeriveName.DeriveNameProp(DeriveName.DeriveInitially)))

  lazy val Page = ThingState(PageOID, systemOID, SimpleThingOID,
    toProps(
      setName("Simple-Page"),
      IsModelProp(true),
      DeprecatedProp(true)))

  lazy val PhotoBase = ThingState(PhotoBaseOID, systemOID, querki.basic.MOIDs.SimpleThingOID,
    toProps(
      setName("Photograph-Base"),
      IsModelProp(true),
      DisplayTextProp("""
This is the Model for all uploaded photographs. You shouldn't try to base something on this directly --
just upload a photograph, and you'll get one of these.
""")),
    querki.time.epoch,
    Kind.Attachment
    )

object Bulleted extends ThingState(BulletedOID, systemOID, RootOID,
    toProps(
      setName("_bulleted"),
      ApplyMethod("\"\"* ____\"\""),
      DisplayTextProp("""    LIST -> _bulleted
          |This method takes a LIST, and render its elements as a bullet list, one per line. It is simply syntactic sugar for
          |    LIST -> \""* \____\"" """.stripMargin)))

object Commas extends ThingState(CommasMethodOID, systemOID, RootOID,
    toProps(
      setName("_commas"),
      ApplyMethod("""_join("", "")"""),
      DisplayTextProp("""    LIST -> _commas
          |This method takes a LIST, and render its elements comma-separated. It is simply syntactic sugar for
          |    LIST -> _join(\"", \"")""".stripMargin)))

object DisplayThingTree extends ThingState(DisplayThingTreeOID, systemOID, RootOID,
    toProps(
      setName("_displayThingTree"),
      ApplyMethod("""""[[_if(_isModel, ""{{_modelInTree:"")]]____[[_if(_isModel, "" [[_createInstanceLink -> _iconButton(""icon-plus-sign"", ""Create an Instance"")]]}}"")]]
{{indent:[[_children -> 
  _sort -> 
  _displayThingTree]]
}}
""""")))

object AllThings extends ThingState(AllThingsOID, systemOID, RootOID,
    toProps(
      setName("All Things"),
      DisplayTextProp("[[All Things]]"),
      ApplyMethod("""""{{_thingTree:
[[_currentSpace ->
  _externalRoots ->
  _sort ->
  _displayThingTree]]
}}""""")))

object AllProps extends ThingState(AllPropsThingOID, systemOID, RootOID,
    toProps(
      setName("All Properties"),
      DisplayTextProp("[[All Properties]]"),
      ApplyMethod("""""{{_thingTree:
[[_currentSpace ->
  _allProps ->
  _bulleted]]
}}
""""")))

  override lazy val things = Seq(
    SimpleThing,
    Page,
    PhotoBase,
    Bulleted,
    Commas,
    DisplayThingTree,
    AllThings,
    AllProps
  )
}