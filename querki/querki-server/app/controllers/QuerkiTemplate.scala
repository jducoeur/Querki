package controllers

object QuerkiTemplate extends Enumeration {
  type QuerkiTemplate = Value
  
  val Admin, Edit, Index, Login, NewSpace, Profile, SpaceList, Thing, Things, Upload, View = Value
}