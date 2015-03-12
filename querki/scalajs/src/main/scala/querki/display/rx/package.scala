package querki.display

package object rx {
  sealed trait AttrVal

  implicit def strToAttrVal(v:String):AttrVal = v.asInstanceOf[AttrVal]
  implicit def intToAttrVal(v:Int):AttrVal = v.asInstanceOf[AttrVal]
  implicit def boolToAttrVal(v:Boolean):AttrVal = v.asInstanceOf[AttrVal]
}
