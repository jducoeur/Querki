package querki.imexport

import models.{OID, Property, PropertyBundle, Thing}

import querki.ecology._
import querki.types.{ModelTypeBase, ModeledPropertyBundle}
import querki.values.{QValue, SpaceState}

trait PropAccessor {
  def getPropValue(instanceOpt: Option[PropertyBundle])(implicit state: SpaceState): Seq[QValue]

  def getTitles: Seq[String]
}

case class SimplePropAccessor(prop: Property[_, _]) extends PropAccessor {

  def getPropValue(instanceOpt: Option[PropertyBundle])(implicit state: SpaceState): Seq[QValue] = {
    val vOpt = for {
      instance <- instanceOpt
      pv <- instance.getPropOpt(prop)
    } yield pv.v

    vOpt.map(Seq(_)).getOrElse(Seq())
  }

  def getTitles: Seq[String] = Seq(prop.unsafeDisplayName)
}

case class ModelPropAccessor(
  prop: Property[ModeledPropertyBundle, _],
  children: Seq[PropAccessor]
) extends PropAccessor {

  def getPropValue(instanceOpt: Option[PropertyBundle])(implicit state: SpaceState): Seq[QValue] = {
    val vOpt = for {
      instance <- instanceOpt
      pv <- instance.getPropOpt(prop)
      // NOTE: we only use the *first* element here, and we always guarantee that we will use one. This is conceptually
      // broken, but necessary in order to get "square" results.
      // Note also that we have to go directly to the pType to get the fallback value, because we want to guarantee
      // that we get exactly one value. If the property is a List, then the default will still be empty:
      v <- pv.firstOpt.orElse(Some(prop.pType.doDefault))
    } yield v

    children.flatMap { child =>
      child.getPropValue(vOpt)
    }
  }

  def getTitles: Seq[String] = children.flatMap(_.getTitles)
}

case class CategoryPropAccessor(
  prop: Property[OID, _],
  values: Seq[Thing]
)(implicit
  val ecology: Ecology
) extends PropAccessor
     with EcologyMember {
  lazy val Core = interface[querki.core.Core]
  lazy val LinkType = Core.LinkType
  lazy val ExactlyOne = Core.ExactlyOne
  lazy val TextType = Core.TextType

  def getPropValue(instanceOpt: Option[PropertyBundle])(implicit state: SpaceState): Seq[QValue] = {
    val vOpt = for {
      instance <- instanceOpt
      pv <- instance.getPropOpt(prop)
    } yield pv.v

    val strs = values.map { thing =>
      val id = thing.id
      // TODO: this is kind of a broken abstraction break, the fact that we are defining the
      // "checkboxes" here. They really ought to be defined in the Exporter itself. Think about how
      // to better handle this.
      if (vOpt.isDefined && vOpt.get.contains(LinkType, id))
        "x"
      else
        ""
    }

    strs.map(str => ExactlyOne(TextType(str)))
  }

  def getTitles: Seq[String] = values.map(_.displayName)
}

/**
 * Utility trait for Exporters that export their data in columnar formats.
 */
private[imexport] trait SquareExporter extends EcologyMember {
  lazy val Core = interface[querki.core.Core]
  lazy val Links = interface[querki.links.Links]
  lazy val LinkType = Core.LinkType
  lazy val QSet = Core.QSet

  def columns(model: Thing)(implicit state: SpaceState): Seq[PropAccessor] = {
    val props = interface[querki.editing.Editor].instancePropsForModel(model, state)

    props.map { prop =>
      prop.pType match {
        case mt: ModelTypeBase => {
          val modelProp = prop.asInstanceOf[Property[ModeledPropertyBundle, _]]
          val modelOpt = state.anything(mt.basedOn)
          modelOpt match {
            case Some(model) => ModelPropAccessor(modelProp, columns(model))
            case None        => SimplePropAccessor(modelProp)
          }
        }

        case _ => {
          // If this Property is a Set of Links to a Category, then we break it out to separate columns for the
          // Category values:
          val categoryOpt = for {
            linkProp <- prop.confirmType(LinkType)
            if (prop.cType == QSet)
            linkModelPV <- prop.getPropOpt(Links.LinkModelProp)
            linkModelId <- linkModelPV.firstOpt
            linkModel <- state.anything(linkModelId)
            if (linkModel.ifSet(Links.NoCreateThroughLinkProp))
          } yield CategoryPropAccessor(linkProp, state.descendants(linkModelId, false, true).toSeq.sortBy(_.displayName))

          categoryOpt.getOrElse(SimplePropAccessor(prop))
        }
      }
    }
  }
}
