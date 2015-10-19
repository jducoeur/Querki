package querki.types

import scala.xml.NodeSeq

import models.{DisplayPropVal, OID, Property, PropertyBundle, PropertyBundleOps, PType, PTypeBuilder, Thing, Wikitext}
import models.Thing.{PropMap, emptyProps}

import querki.ecology._
import querki.globals._
import querki.util.{QLog, XmlHelpers}
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
case class ModeledPropertyBundle(val modelType:ModelTypeDefiner#ModelType, basedOn:OID, props:PropMap)(implicit val ecology:Ecology) 
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
  
  override def toString = {
    "ModeledPropertyBundle " + modelType.displayName + ": " + props
  }
  
  def thingOps(e:Ecology):PropertyBundleOps = new ModelTypeOps(this)(e)
}

class ModelTypeOps(bundle:ModeledPropertyBundle)(implicit e:Ecology) extends PropertyBundleOps(bundle) {
  def thisAsQValue:QValue = interface[querki.core.Core].ExactlyOne(ElemValue(bundle, bundle.modelType))  
}

/**
 * This mix-in trait is intended for use by any system that needs to create ModelTypes -- not *often* needed, but there
 * are several places that need to be able to do it.
 */
trait ModelTypeDefiner { self:EcologyMember =>
  
  private lazy val Core = interface[querki.core.Core]
  
  class ModelType(tid:OID, val basedOn:OID, typeProps:PropMap) extends querki.core.TypeUtils.SystemType[ModeledPropertyBundle](tid,
      typeProps) with PTypeBuilder[ModeledPropertyBundle, SimplePropertyBundle] with ModelTypeBase
  {
    override lazy val props:PropMap = propFetcher + 
		  (ModelForTypePropOID -> Core.ExactlyOne(Core.LinkType(basedOn)))

    lazy val Basic = interface[querki.basic.Basic]
    lazy val Editor = interface[querki.editing.Editor]
    lazy val QL = interface[querki.ql.QL]
    lazy val SpacePersistence = interface[querki.spaces.SpacePersistence]
    
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
    def doWikify(context:QLContext)(v:ModeledPropertyBundle, displayOpt:Option[Wikitext], lexicalThing:Option[PropertyBundle] = None) = {
      implicit val state = context.state
      v.getPropOpt(Basic.DisplayTextProp) match {
        case Some(defaultViewPV) if (!(defaultViewPV.isEmpty)) => {
          // The Type's Model has a Default View, so use that:
          val defaultView = defaultViewPV.first
          QL.process(defaultView, v.thisAsContext(context.request, state, ecology))
        }
        
        case _ => {
          // There's no Default View, so simply render this as key/value pairs
          
          // Introduce a bit of indirection, so we can sort the properties by display name:
          val propInfo = relevantProps(v).map { pair =>
            val (propId, propVal) = pair
            (propId, state.anything(propId), propVal)
          }
          val sortedInfos = propInfo.toSeq.sortBy(_._2.map(_.displayName).getOrElse(""))
          val result = (Future.successful(Wikitext.empty) /: propInfo) { (current, pair) =>
            val (propId, propOpt, propVal) = pair
            val propText = propOpt match {
              case Some(prop) => {
                propVal.wikify(context, displayOpt, lexicalThing) map { Wikitext(": " + prop.displayName + " : ") + _ } 
              }
              case None => Future.successful(Wikitext("Unknown property " + propId))
            }
            
            // Okay, we now have an incoming Future and a newly-created one. FlatMap them to get the result:
            for {
              c <- current
              p <- propText
            }
              yield c.+(p, true)
          }
          result
        }
      }
    }
    
    def doDefault(implicit state:SpaceState) = { 
      state.anything(basedOn) match {
        // The "default value" for a Model Type simply passes through to the Model. Note that this
        // is a change from the old code, which copied the props from the model into the bundle. That
        // was wrong, because it meant that changes to the Model's values didn't pass through.
        case Some(model) => ModeledPropertyBundle(this, basedOn, Thing.emptyProps)
        case None => throw new Exception(s"Model $basedOn for Model Type $id no longer exists!")
      }
    }
    
    def wrap(raw:SimplePropertyBundle):ModeledPropertyBundle = {
      ModeledPropertyBundle(this, basedOn, raw.props)
    }
    
    override def renderInputXml(prop:Property[_,_], context:QLContext, currentValue:DisplayPropVal, v:ElemValue):Future[NodeSeq] = {
      val bundle = get(v)
      Editor.getInstanceEditor(bundle, context, Some(currentValue)).map { wikitext =>
        XmlHelpers.toNodes(wikitext.display)
      }
    }
  }
  
}