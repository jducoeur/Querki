package org.querki.shocon

sealed trait HCValue

case class SimpleValue(v:String) extends HCValue

case class ObjectValue(vs:Map[String, HCValue]) extends HCValue
