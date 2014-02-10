package querki.types

import scala.xml.Elem

import models.{DisplayPropVal, OID, Property, PropertyBundle, PType, PTypeBuilder, Thing, Wikitext}
import models.Thing.{PropMap, emptyProps}

import querki.ecology._
import querki.util.QLog
import querki.values.{ElemValue, PropAndVal, QLContext, QValue, RequestContext, SpaceState}

import MOIDs._

case class SimplePropertyBundle(props:PropMap)

object SimplePropertyBundle {
  def apply(vals:(OID, QValue)*):SimplePropertyBundle = {
    SimplePropertyBundle(emptyProps ++ vals)
  }
}

/**
 * TODO: a good deal of this code is copied from Thing. Think carefully about the right factoring here. I kind of
 * want PropertyBundle to remain a pure interface, but we may want to carefully lift out a base implementation.
 */
case class ModeledPropertyBundle(modelType:ModelTypeDefiner#ModelType, basedOn:OID, props:PropMap)(implicit val ecology:Ecology) 
  extends PropertyBundle with EcologyMember 
{
  def isThing:Boolean = false
  def asThing:Option[Thing] = None
  def hasModel:Boolean = true
  def getModel(implicit state:SpaceState):Thing = {
    getModelOpt(state).getOrElse(throw new Exception(s"Trying to fetch Model $basedOn for Model Type $modelType, but it doesn't seem to exist!"))
  }
  
  def getModelOpt(implicit state:SpaceState):Option[Thing] = {
    state.anything(modelType.basedOn)
  }
  
  def thisAsContext(implicit request:RequestContext):QLContext = QLContext(interface[querki.core.Core].ExactlyOne(ElemValue(this, modelType)), Some(request))
  
  def hasProp(propId:OID)(implicit state:SpaceState):Boolean = {
    props.contains(propId) || { 
      getModelOpt.map(_.hasProp(propId)).getOrElse(false)
    }
  }
  
  def getPropOpt[VT](prop:Property[VT, _])(implicit state:SpaceState):Option[PropAndVal[VT]] = {
    if (hasProp(prop))
      Some(getProp(prop))
    else
      None
  }
  
  def getPropVal[VT, CT](prop:Property[VT, _])(implicit state:SpaceState):QValue = {
    localPropVal(prop).getOrElse {
      getModelOpt.map(_.getPropVal(prop)).getOrElse { 
        modelType.interface[querki.ql.QL].WarningValue(s"Couldn't find Property ${prop.displayName} on the received value")
      }
    }
  }

  def localProp[VT, CT](prop:Property[VT, _]):Option[PropAndVal[VT]] = {
    prop.fromOpt(this.props) map prop.pair
  }
  
  def getProp[VT, CT](prop:Property[VT, _])(implicit state:SpaceState):PropAndVal[VT] = {
    localProp(prop).getOrElse(getModelOpt.map(_.getProp(prop)).getOrElse(prop.defaultPair))
  }
  
  def map[VT, DT, RT](prop:Property[VT, _], destType:PType[DT] with PTypeBuilder[DT, RT])(cb:VT => RT)(implicit state:SpaceState):QValue = {
    val propAndVal = getProp(prop)
    propAndVal.map(destType)(cb)    
  }
}

/**
 * This mix-in trait is intended for use by any system that needs to create ModelTypes -- not *often* needed, but there
 * are several places that need to be able to do it.
 */
trait ModelTypeDefiner { self:EcologyMember =>
  
  class ModelType(tid:OID, val basedOn:OID, typeProps:() => PropMap) extends querki.core.TypeUtils.SystemType[ModeledPropertyBundle](tid,
      typeProps) with PTypeBuilder[ModeledPropertyBundle, SimplePropertyBundle]
  {
    override lazy val props:PropMap = propFetcher() + 
		  (ModelForTypePropOID -> Core.ExactlyOne(Core.LinkType(basedOn)))
    
    lazy val SpacePersistence = interface[querki.spaces.SpacePersistence]
    lazy val Editor = interface[querki.editing.Editor]
    
    override def editorSpan(prop:Property[_,_]):Int = 12
    
    def doDeserialize(v:String)(implicit state:SpaceState) = { 
      ModeledPropertyBundle(this, basedOn, SpacePersistence.deserializeProps(v, state))
    }
    def doSerialize(v:ModeledPropertyBundle)(implicit state:SpaceState) = { 
      SpacePersistence.serializeProps(relevantProps(v), state)
    }
    
    private def relevantProps(v:ModeledPropertyBundle)(implicit state:SpaceState):Map[OID, QValue] = {
      relevantProps(v.getModel, v)
    }
    
    private def relevantProps(model:Thing, v:PropertyBundle)(implicit state:SpaceState):Map[OID, QValue] = {    
      def instancePropsOpt = model.getPropOpt(Editor.InstanceProps)
      instancePropsOpt match {
        case Some(instancePropsPV) => {
          val instanceProps = instancePropsPV.v.rawList(Core.LinkType)
          v.props.filter { pair => 
            val (propId, _) = pair
            instanceProps.contains(propId)
          }
        }
        case None => v.props
      }      
    }
    
    /**
     * By and large, we don't recommend simply displaying a Model Type, since the results are a bit unpredictable. But it should at
     * least be possible to do so.
     * 
     * TODO: in principle, it should be possible to use __stuff__ syntax to define how you want this to render. This is actually
     * hampered by the fact that we've already wikitexted it by this point. Should we be passing the AST into here instead of the
     * wikitext?
     */
    def doWikify(context:QLContext)(v:ModeledPropertyBundle, displayOpt:Option[Wikitext] = None) = {
      implicit val state = context.state
      // Introduce a bit of indirection, so we can sort the properties by display name:
      val propInfo = relevantProps(v).map { pair =>
        val (propId, propVal) = pair
        (propId, state.anything(propId), propVal)
      }
      val sortedInfos = propInfo.toSeq.sortBy(_._2.map(_.displayName).getOrElse(""))
      (Wikitext.empty /: propInfo) { (current, pair) =>
        val (propId, propOpt, propVal) = pair
        val propText = propOpt match {
          case Some(prop) => {
            Wikitext(": " + prop.displayName + " : ") + propVal.wikify(context, displayOpt)
          }
          case None => Wikitext("Unknown property " + propId)
        }
        current.+(propText, true)
      }
    }
    
    def doDefault(implicit state:SpaceState) = { 
      state.anything(basedOn) match {
        // The defaults for this Type are exactly the values defined in the Model it is based on:
        case Some(model) => ModeledPropertyBundle(this, basedOn, relevantProps(model, model))
        case None => throw new Exception(s"Model $basedOn for Model Type $id no longer exists!")
      }
    }
    
    def wrap(raw:SimplePropertyBundle):ModeledPropertyBundle = {
      ModeledPropertyBundle(this, basedOn, raw.props)
    }
    
    override def renderInputXml(prop:Property[_,_], rc:RequestContext, currentValue:DisplayPropVal, v:ElemValue):Elem = {
      val bundle = get(v)
      val wikitext = Editor.getInstanceEditor(bundle, rc, Some(currentValue))
      
      // TODO: this is horrible. How can we fix this abstraction break? The underlying problem is the
      // fact that we are trying to embed generated Wikitext inside of an XML Elem, and we have no concept
      // of that in the design.
      val rawHtml = wikitext.display.html.body
      val nodeSeq = scala.xml.parsing.XhtmlParser(scala.io.Source.fromString(rawHtml))
      nodeSeq.head.asInstanceOf[Elem]
    }
  }
  
}