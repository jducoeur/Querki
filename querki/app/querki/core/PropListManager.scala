package querki.core

import collection.immutable.TreeMap

import models.{DisplayPropVal, Property, Thing}

import models.system.NameProp
import querki.core.MOIDs.UrPropOID

import querki.ecology._

import querki.values.SpaceState

object PropListMOIDs extends EcotIds(20)

class PropListManagerEcot(e:Ecology) extends QuerkiEcot(e) with PropListManager {
  
  lazy val DisplayNameProp = interface[querki.basic.Basic].DisplayNameProp
  lazy val NotInheritedProp = interface[querki.core.Core].NotInheritedProp
  
  implicit object PropNameOrdering extends Ordering[Property[_,_]] {
    def compare(a:Property[_,_], b:Property[_,_]) = {
      if (a eq DisplayNameProp) {
        if (b eq DisplayNameProp)
          0
        else
          // Display Name always displays first
          -1
      } else if (b eq DisplayNameProp)
        1
      else if (a eq NameProp) {
        if (b eq NameProp)
          0
        else
          // Name always displays next
          -1
      } else if (b eq NameProp)
        1
      else
        a.displayName compare b.displayName
    }
  }

  
    def apply(pairs:(Property[_,_], DisplayPropVal)*):PropList = {
      (TreeMap.empty[Property[_,_], DisplayPropVal] /: pairs)((m, pair) => m + pair)
    }
    
    private def isProperty(thing:Option[Thing])(implicit state:SpaceState):Boolean = {
      thing.map(_.isAncestor(UrPropOID)).getOrElse(false)
    }
    
    def inheritedProps(thing:Option[Thing], model:Thing)(implicit state:SpaceState):PropList = {
      // Get the Model's PropList, and push its values into the inherited slots:
      val raw = fromRec(model, thing)   
      (TreeMap.empty[Property[_,_], DisplayPropVal] /: raw) { (result, fromModel) =>
        val (prop, v) = fromModel
        if (prop == NameProp && !isProperty(thing)) {
          // We don't inherit even the existence of NameProp, *except* for Properties.
          // TBD: we might generalize this concept of "use the Name Property primarily" into a
          // meta-Property, but let's see if we care first.
          result
        } else if (prop.first(NotInheritedProp))
          result + (prop -> DisplayPropVal(thing, prop, None))
        else if (v.v.isDefined)
          result + (prop -> DisplayPropVal(thing, prop, None, v.v, Some(model)))
        else
          result + fromModel        
      }   
    }
    
    // TODO: this is all pretty inefficient. We should be caching the PropLists,
    // especially for common models.
    private def fromRec(thing:Thing, root:Option[Thing])(implicit state:SpaceState):PropList = {
      val inherited =
        if (thing.hasModel)
          inheritedProps(root, thing.getModel)
        else
          TreeMap.empty[Property[_,_], DisplayPropVal]
      
      (inherited /: thing.props.keys) { (m, propId) =>
        val propOpt = state.prop(propId)
        propOpt match {
          case Some(prop) => {
            val value = prop.from(thing.props)
            val disp =
              if (m.contains(prop))
                m(prop).copy(v = Some(value))
              else
                DisplayPropVal(root, prop, Some(value))
            m + (prop -> disp)
          }
          case None => m
        }
      }
    }
    
    def from(thing:Thing)(implicit state:SpaceState):PropList = {
      fromRec(thing, Some(thing))
    }
}