package querki.core

import collection.immutable.TreeMap

import models.{DisplayPropVal, Property, PropertyBundle, Thing}

import querki.core.MOIDs.UrPropOID

import querki.ecology._
import querki.globals._
import querki.util.QLog
import querki.values.SpaceState

object PropListMOIDs extends EcotIds(20)

class PropListManagerEcot(e:Ecology) extends QuerkiEcot(e) with PropListManager {
  
  lazy val DisplayNameProp = interface[querki.basic.Basic].DisplayNameProp
  lazy val Editor = interface[querki.editing.Editor]
  lazy val NotInheritedProp = Core.NotInheritedProp
  lazy val NameProp = Core.NameProp
  
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
    
    private def isProperty(bundleOpt:Option[PropertyBundle])(implicit state:SpaceState):Boolean = {
      val resultOpt = for (
        bundle <- bundleOpt;
        thing <- bundle.asThing
      )
        yield thing.isAncestor(UrPropOID)
        
      resultOpt.getOrElse(false)
    }
    
    def inheritedProps(thing:Option[PropertyBundle], model:Thing)(implicit state:SpaceState):PropList = {
      // Get the Model's PropList, and push its values into the inherited slots:
      val raw = fromRec(model, thing)   
      (TreeMap.empty[Property[_,_], DisplayPropVal] /: raw) { (result, fromModel) =>
        val (prop, v) = fromModel
        if (prop == NameProp && !isProperty(thing)) {
          // We don't inherit even the existence of NameProp, *except* for Properties.
          // TBD: we might generalize this concept of "use the Name Property primarily" into a
          // meta-Property, but let's see if we care first.
          result
        } else if (prop.ifSet(Core.ModelOnlyProp))
          // This is a Property whose *existence* is specifically not inherited:
          result
        else if (prop.first(NotInheritedProp))
          // This is a Property whose *value* is not inherited:
          result + (prop -> DisplayPropVal(thing, prop, None))
        else if (v.v.isDefined)
          result + (prop -> DisplayPropVal(thing, prop, None, v.v, Some(model)))
        else
          result + fromModel        
      }   
    }
    
    // TODO: this is all pretty inefficient. We should be caching the PropLists,
    // especially for common models.
    private def fromRec(thing:PropertyBundle, root:Option[PropertyBundle])(implicit state:SpaceState):PropList = {
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
    
    def from(thing:PropertyBundle, ensureName:Boolean)(implicit state:SpaceState):PropList = {
      val raw = fromRec(thing, Some(thing))
      if (ensureName) {
        // The semantic-level calling code has asked us to make sure the NameProp is there.
        // This is ugly coupling: we should look for a better way for EditFunctionsImpl to be
        // able to ensure Name is there when it needs it.
        if (raw.contains(Core.NameProp)) {
          raw
        } else {
          raw + (Core.NameProp -> DisplayPropVal(Some(thing), Core.NameProp, None))
        }
      } else
        raw
    }

  // TODO: this overlaps horribly with code in EditorModel. This determines the Properties in the Advanced Editor; that has the ones
  // in the Instance Editor. Merge them together!
  def prepPropList(propList:PropList, thingOpt:Option[PropertyBundle], model:Thing, state:SpaceState, forceName:Boolean = false):Seq[(Property[_,_], DisplayPropVal)] = {
    val propsToEdit = model.getPropOpt(Editor.InstanceProps)(state).map(_.rawList)
    propsToEdit match {
      // If the model specifies which properties we actually want to edit, then use just those, in that order:
      case Some(editList) => {
        val fullEditList = thingOpt match {
          case Some(thing) => 
            editList ++ 
            Editor.propsNotInModel(thing, state) ++
            // HACK: if Name is in propList but *not* in the Instance Props, it usually won't show up in propsNotInModel
            // because it usually *is* set there. But since it's not inherited, it gets overlooked. So we add it by hand here.
            // This is ugly and horrible. We should consider changing propsNotInModel to also return any that are
            // non-inherited, but I am not at all sure that's correct, so think about it carefully.
            (if (forceName && propList.contains(Core.NameProp) && !editList.contains(Core.NameProp.id)) List(Core.NameProp.id) else List.empty)
          case None => editList
        }
        val withOpts = (Seq.empty[(Property[_,_], Option[DisplayPropVal])] /: fullEditList) { (list, oid) =>
          val propOpt = state.prop(oid)
          propOpt match {
            case Some(prop) => {
              val v = propList.get(prop)
              list :+ (prop, v)
            }
            case None => {
              QLog.error("Was unable to find Property " + oid + " in prepPropList()")
              list
            }
          }
        }
        withOpts.filter(_._2.isDefined).map(pair => (pair._1, pair._2.get))
      }
      // Otherwise, we default to doing it by name:
      case None => propList.toList
    }
  }

}