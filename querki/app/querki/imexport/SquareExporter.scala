package querki.imexport

import models.{Property, PropertyBundle, Thing}

import querki.ecology._
import querki.types.{ModeledPropertyBundle, ModelTypeBase}
import querki.values.{QValue, SpaceState}

trait PropAccessor {
  def getPropValue(instanceOpt:Option[PropertyBundle])(implicit state:SpaceState):Seq[QValue]
  
  def getTitles:Seq[String]
}

case class SimplePropAccessor(prop:Property[_,_]) extends PropAccessor {
  def getPropValue(instanceOpt:Option[PropertyBundle])(implicit state:SpaceState):Seq[QValue] = {
    val vOpt = for {
      instance <- instanceOpt
      pv <- instance.getPropOpt(prop)
    }
      yield pv.v
      
    vOpt.map(Seq(_)).getOrElse(Seq())
  }
  
  def getTitles:Seq[String] = Seq(prop.unsafeDisplayName)
}

case class ModelPropAccessor(prop:Property[ModeledPropertyBundle,_], children:Seq[PropAccessor]) extends PropAccessor {
  def getPropValue(instanceOpt:Option[PropertyBundle])(implicit state:SpaceState):Seq[QValue] = {
    val vOpt = for {
      instance <- instanceOpt
      pv <- instance.getPropOpt(prop)
      v <- pv.firstOpt
    }
      yield v
      
    children.flatMap { child =>
      child.getPropValue(vOpt)
    }
  }
  
  def getTitles:Seq[String] = children.flatMap(_.getTitles)
}

/**
 * Utility trait for Exporters that export their data in columnar formats.
 */
private[imexport] trait SquareExporter extends EcologyMember {
  def columns(model:Thing)(implicit state:SpaceState):Seq[PropAccessor] = {
    val props = interface[querki.editing.Editor].instancePropsForModel(model, state)
    
    props.map { prop =>
      prop.pType match {
        case mt:ModelTypeBase => {
          val modelProp = prop.asInstanceOf[Property[ModeledPropertyBundle,_]]
          val modelOpt = state.anything(mt.basedOn)
          modelOpt match {
            case Some(model) => ModelPropAccessor(modelProp, columns(model))
            case None => SimplePropAccessor(modelProp)
          }
        }
        
        case _ => SimplePropAccessor(prop)
      }
    }
  }
}