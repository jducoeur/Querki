package models

import querki.values.{QValue, SpaceState}

class FieldIds(bundleOpt:Option[PropertyBundle], val p:Property[_,_], val container:Option[FieldIds] = None, val index:Option[Int] = None) {
  lazy val propId = p.id.toString
  lazy val propIdWithIndex = propId  + index.map("[" + _.toString + "]").getOrElse("")
  
  def idStack(parent:Option[FieldIds], soFar:String, withThing:Boolean):String = {
    val wrappedSoFar = {
      if (soFar.length() > 0)
        propIdWithIndex + "-" + soFar
      else
        propIdWithIndex
    }
    parent match {
      case Some(p) => p.idStack(p.container, wrappedSoFar, withThing)
      case None => wrappedSoFar + { if (withThing) "-" + plainThingId else "" }
    }
  }
  
  lazy val fullPropId = idStack(container, "", false)
  
  /**
   * Note that this produces an undotted version of the name, whereas thingId produces the
   * more normal format.
   */
  private lazy val plainThingId:String = {
    val resultOpt = for (
      bundle <- bundleOpt;
      thing <- bundle.asThing
    )
      yield thing.id.toString
      
    resultOpt.orElse(container.map(_.plainThingId)).getOrElse("")
  }
  
  lazy val thingId:Option[String] = {
    val resultOpt:Option[String] = for (
      bundle <- bundleOpt;
      thing <- bundle.asThing
    )
      yield thing.id.toThingId
      
    resultOpt.orElse(container.flatMap { cont => cont.thingId }) 
  }
  
  lazy val suffix = "-" + idStack(container, "", true)
  
  lazy val inputControlId = "v" + suffix
  lazy val collectionControlId = "coll" + suffix
  // This is a hidden input field, which is a flag to tell the receiving code whether the
  // field is "empty" -- inherited or deleted, but with no local value:
  lazy val emptyControlId = "empty" + suffix
  
  override def toString = idStack(container, "", true)
}
object FieldIds {
  def apply(t:Option[Thing], p:Property[_,_]) = new FieldIds(t,p)
}

case class DisplayPropVal(on:Option[PropertyBundle], prop: Property[_,_], v: Option[QValue], 
    inheritedVal:Option[QValue] = None, inheritedFrom:Option[Thing] = None, cont:Option[DisplayPropVal] = None, i:Option[Int] = None) 
  extends FieldIds(on, prop, cont, i)
{
  lazy val isInherited = v.isEmpty && inheritedVal.isDefined
  
  lazy val hasInheritance = inheritedVal.isDefined
  
  lazy val effectiveV = v.orElse(inheritedVal)
}

object DisplayPropVal {
  private def propPathFromSuffix(suffixIn:String, bundle:Option[PropertyBundle])(implicit state:SpaceState):Option[FieldIds] = {
    // HACK: to work around the funny values produced by Manifest, when it is dynamically serializing.
    // Fix this on the client side!
    val suffix = 
      if (suffixIn.contains("_values["))
        suffixIn.substring(0, suffixIn.indexOf("_values["))
      else
        suffixIn
        
    val path = suffix.split("-").map(IndexedOID.parse(_))
    if (path.exists(_.isEmpty))
      // Something didn't parse, so it's not a legal path:
      None
    else {
      // Drop the Thing ID at the right-hand end:
      val propIds =
        if (suffix.endsWith("-"))
          // There is no ThingID -- this is common when you are creating a new Thing:
          path.flatten
        else
          path.flatten.dropRight(1)
      (Option.empty[FieldIds] /: propIds) { (current, propId) =>
        val prop = state.prop(propId.id).get
        Some(new FieldIds(bundle, prop, current, propId.i))
      }
    }
  }
  
  /**
   * Given a field name from the public document (which might originate from inputControlId or emptyControlId),
   * try to parse it, and pull out the "property" part of the name.
   * 
   * This is used in the Application code, to figure out which properties were being edited in the browser.
   */
  def propPathFromName(publicName:String, bundle:Option[PropertyBundle])(implicit state:SpaceState):Option[FieldIds] = {
    if (publicName.startsWith("v-"))
      propPathFromSuffix(publicName.substring(2), bundle)
    else if (publicName.startsWith("empty-"))
      propPathFromSuffix(publicName.substring(6), bundle)
    else
      // This clearly isn't the right format, on its face:
      None
  }
}