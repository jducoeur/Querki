package querki.uservalues

import scala.concurrent.Future
import scala.reflect.runtime.universe._
import scala.xml.NodeSeq

import models.{DisplayPropVal, OID, Property, PropertyBundle, PType, PTypeBuilder, SimplePTypeBuilder, UnknownOID, Wikitext}

import querki.core.TypeUtils.DiscreteType
import querki.ecology._
import querki.globals._
import querki.types.{ModelTypeBase, PropPath}
import querki.uservalues.PersistMessages.OneUserValue
import querki.util.QLog
import querki.values.{ElemValue, QLContext, QValue, RequestContext, SpaceState}

case class DiscreteSummary[UVT](propId:OID, content:Map[UVT,Int]) 
{
  override def equals(other:Any):Boolean = {
    other match {
      case otherSummary:DiscreteSummary[UVT] => (otherSummary.propId == propId) && (otherSummary.content == content) 
      case _ => false
    }
  }
}

/**
 * Describes a mechanism for summarizing the User Values for a Property.
 */
trait Summarizer[UVT,VT] {
  /**
   * Based on the previous and current User Values for a single User, produce an updated summary
   * value for this Thing.
   */
  def addToSummary(tid:OID, fromProp:Property[_,_], prop:Property[VT,_], previous:Option[QValue], current:Option[QValue])(implicit state:SpaceState):QValue
  
  /**
   * Goes through all of the existing UserValues, and recomputes their summaries. Returns the *changes* that need to be made, as ThingId/Value pairs.
   */
  def recalculate(fromProp:Property[_,_], prop:Property[VT,_], values:Seq[OneUserValue])(implicit state:SpaceState):Iterable[(OID, QValue)]
}

trait SummarizerDefs { self:QuerkiEcot =>
  lazy val PropPaths = interface[querki.types.PropPaths]
  // Must be declared in the actual Ecot:
  def UserValues:UserValues
  
  lazy val SummarizesPropertyLink = UserValues.SummarizesPropertyLink
  
  /**
   * Base implementation for all Summarizers.
   * 
   * Note that this does *not* derive from SystemType, mainly because I want there to be a common Model
   * for Summarizers.
   */
  abstract class SummarizerBase[UVT,VT](tid:OID, pf:PropFetcher)
    extends PType[VT](tid, SystemIds.systemOID, MOIDs.SummarizerBaseOID, pf) with Summarizer[UVT,VT]
    with SimplePTypeBuilder[VT]
  {
    // HACK: so far, I haven't come up with a compile-time way to deal with this, so we need to do the typechecking at runtime.
    // The problem is that fromProp *might* be of UVT, but it might also be a ModelType.
    // TODO: this really ought to be typechecking that elem *is* a UVT, but erasure makes that hard. How do we do this? So far,
    // my attempts with TypeTags haven't worked.
    def castToUVT(vOpt:Option[QValue]):Option[UVT] = {
      try {
	      for {
	        v <- vOpt
	        first <- v.firstOpt
	        elemV = first.elem
	      }
	        yield elemV.asInstanceOf[UVT]
      } catch {
        // TODO: this needs to become *much* better:
        case ex:Exception => None
      }
    }
    
    def rightPath(paths:Seq[PropPath[_,_]], bundle:PropertyBundle)(implicit state:SpaceState):Option[PropPath[_,_]] = {
      paths.find(path => !(path.getPropOpt(bundle).isEmpty))
    }
    
    def viaPath(bundleOpt:Option[PropertyBundle], path:PropPath[_,_])(implicit state:SpaceState):Option[UVT] = {
      for {
        bundle <- bundleOpt
        pv <- path.getPropOpt(bundle).headOption
        v = pv.v
        result <- castToUVT(Some(v))
      }
        yield result
    }
    
    // ===========================================================
    
    def addToSummary(tid:OID, fromProp:Property[_,_], prop:Property[VT,_], previous:Option[QValue], current:Option[QValue])(implicit state:SpaceState):QValue = {
      val resultViaPath = fromProp.pType match {
        case mt:ModelTypeBase => {
	      for {
	        fromBundleProp <- fromProp.confirmType(mt)
	        summarizesPropPV <- prop.getPropOpt(SummarizesPropertyLink)
	        summarizesPropId <- summarizesPropPV.firstOpt
	        summarizesProp <- state.prop(summarizesPropId)
	        previousBundle = previous.flatMap(_.firstAs(mt))
	        currentBundle = current.flatMap(_.firstAs(mt))
	        checkBundle <- previousBundle orElse currentBundle 
	        path <- rightPath(PropPaths.pathsToProperty(summarizesProp), checkBundle)
	      }
	        yield ExactlyOne(ElemValue(doAddToSummary(tid, summarizesProp, prop, viaPath(previousBundle, path), viaPath(currentBundle, path)), this))
        }
        case _ => None
      }
      
      resultViaPath.getOrElse(ExactlyOne(ElemValue(doAddToSummary(tid, fromProp, prop, castToUVT(previous), castToUVT(current)), this)))
    }
    def doAddToSummary(tid:OID, fromProp:Property[_,_], prop:Property[VT,_], previous:Option[UVT], current:Option[UVT])(implicit state:SpaceState):VT
    
    // ===========================================================
    
    def recalculate(fromProp:Property[_,_], prop:Property[VT,_], values:Seq[OneUserValue])(implicit state:SpaceState):Iterable[(OID, QValue)] = {
      val pathOpt:Option[(ModelTypeBase, PropPath[_,_], Property[_,_])] = fromProp.pType match {
        case mt:ModelTypeBase => {
	      for {
	        fromBundleProp <- fromProp.confirmType(mt)
	        summarizesPropPV <- prop.getPropOpt(SummarizesPropertyLink)
	        summarizesPropId <- summarizesPropPV.firstOpt
	        summarizesProp <- state.prop(summarizesPropId)
	        checkBundle <- values.find(!_.v.isEmpty).flatMap(_.v.firstAs(mt))
	        path <- rightPath(PropPaths.pathsToProperty(summarizesProp), checkBundle)
	      }
	        yield (mt, path, summarizesProp)
        }
        case _ => None
      }
      
      def recalculateOneThing(tid:OID, values:Seq[OneUserValue]):Option[(OID,QValue)] = {
        pathOpt match {
          case Some((mt, path, summarizesProp)) => {
            val vs = values.map(_.v)
            val bundles = vs.map(_.firstAs(mt))
            val uvts = bundles.map(viaPath(_,path)).flatten
            doRecalculateOneThing(tid, summarizesProp, prop, uvts).map(result => (tid, ExactlyOne(ElemValue(result, this))))
          }
          case None => {
            val vs = values.map(_.v)
            val uvts = vs.map(v => castToUVT(Some(v))).flatten
            doRecalculateOneThing(tid, fromProp, prop, uvts).map(result => (tid, ExactlyOne(ElemValue(result, this))))
          }
        }
      }

      val byThing:Map[OID,Seq[OneUserValue]] = values.groupBy(_.thingId)      
      val recalculations = byThing.map(pair => recalculateOneThing(pair._1, pair._2))
      recalculations.flatten
    }
    // This should return a non-empty result *if* the summary has changed from its existing value:
    def doRecalculateOneThing(tid:OID, fromProp:Property[_,_], prop:Property[VT,_], values:Seq[UVT])(implicit state:SpaceState):Option[VT]
  }
  
  /**
   * This Summarizer is intended for any Type with a limited number of discrete values. The Summary is one
   * integer for each value, giving the number of times it has been specified.
   * 
   * The userType is the PType we are actually summarizing values of. We mostly need it for serialization.
   */
  class DiscreteSummarizer[UVT](tid:OID, userType:PType[UVT], p:PropFetcher) extends SummarizerBase[UVT,DiscreteSummary[UVT]](tid, p) {
	def doAddToSummary(tid:OID, fromProp:Property[_,_], prop:Property[DiscreteSummary[UVT],_], previous:Option[UVT], current:Option[UVT])(implicit state:SpaceState):DiscreteSummary[UVT] = {
	  state.anything(tid) match {
	    case Some(thing) => {
	      thing.getPropOpt(prop) match {
		    case Some(oldSummary) => {
		      // TBD: the "first" below is suspicious. Can it ever be wrong?
		      val oldMap = oldSummary.first.content
		      // First, decrement the previous key, if there was one...
		      val mapWithoutPrevious = previous match {
		        case Some(prevKey) if (oldMap.contains(prevKey)) => {
		          val prevVal = (oldMap(prevKey) - 1)
		          if (prevVal == 0)
		            oldMap - prevKey
		          else
		            oldMap + (prevKey -> prevVal) 
		        }
		        case _ => oldMap
		      }
		      // ... then increment the new key, if there is one:
		      val mapWithCurrent = current match {
		        case Some(curKey) => {
		          if (mapWithoutPrevious.contains(curKey))
		            // We already have records with that key:
		            mapWithoutPrevious + (curKey -> (mapWithoutPrevious(curKey) + 1))
		          else
		            // First time someone's given this value:
		            mapWithoutPrevious + (curKey -> 1)
		        }
		        // We've simply removed the previous value, without giving a new one:
		        case _ => mapWithoutPrevious
		      }
		      DiscreteSummary(fromProp.id, mapWithCurrent)
		    }
		      
		    case None => {
		      current match {
		        // First value for this Thing:
		        case Some(curVal) => DiscreteSummary(fromProp.id, Map(curVal -> 1))
		        // TODO: should this be a warning? Kind of strange to get a "change" that contains no value, if
		        // there wasn't one before. What's the change?
		        case None => DiscreteSummary(fromProp.id, Map())
		      }
		    }
		  }        
	    }
	      
	    case None => {
	      QLog.error(s"Got addToSummary for unknown Thing $tid")
	      DiscreteSummary(fromProp.id, Map())
	    }
	  }
	}
	
	def doRecalculateOneThing(tid:OID, fromProp:Property[_,_], prop:Property[DiscreteSummary[UVT],_], values:Seq[UVT])(implicit state:SpaceState):Option[DiscreteSummary[UVT]] = {
	  val recalculated = {
	    val m = (Map.empty[UVT,Int] /: values) { (totals, v) =>
	      totals.get(v) match {
	        case Some(count) => totals + (v -> (count + 1))
	        case None => totals + (v -> 1)
	      }
	    }
	    DiscreteSummary(fromProp.id, m)
	  }
	  
	  val existingOpt = for {
	    thing <- state.anything(tid)
	    existingPV <- thing.getPropOpt(prop)
	    ex <- existingPV.firstOpt
	  }
	    yield ex
	    
	  existingOpt match {
	    case Some(existing) => {
	      if (existing == recalculated)
	        // No change
	        None
	      else
	        Some(recalculated)
	    }
	    case None => Some(recalculated)
	  }
	}
	  
	def doDeserialize(ser:String)(implicit state:SpaceState):DiscreteSummary[UVT] = {
	  // TODO: this should unescape " - ", "," and ":", so we can cope with arbitrary Strings:
	  val (propId, values) = ser.split(" - ") match {
	    case Array(propIdStr, valStr) => {
	      (OID(propIdStr), valStr)
	    }
	    case Array(valStr) => (UnknownOID, valStr)
	    case _ => (UnknownOID, "")
	  }
	  
	  val pairStrs = values.split(",")
	  // Filter out the empty-string case:
	  val pairs = if (pairStrs.length > 0 && pairStrs(0).length() > 0) pairStrs.map { pairStr =>
	    val parts = pairStr.split(":")
	    if (parts.length != 2)
	        throw new Exception(s"DiscreteSummarizer.doDeserialize got a bad pair: $pairStr")
	      
	    (userType.doDeserialize(parts(0)), java.lang.Integer.parseInt(parts(1)))
	  } else
	    Array.empty[(UVT,Int)]
	    
	  DiscreteSummary(propId, Map(pairs:_*))
	}
	  
	def doSerialize(summary:DiscreteSummary[UVT])(implicit state:SpaceState):String = {
	  val pairStrs = summary.content.map { pair =>
	    val (key, num) = pair
	    userType.doSerialize(key) + ":" + num.toString
	  }
	  summary.propId.toString + " - " + pairStrs.mkString(",")
	}
	
	// Split out so that subclasses can override this:
	def wikifyKey(context:QLContext, fromProp:Option[Property[_,_]], key:UVT):Wikitext = {
	  awaitHack(userType.doWikify(context)(key))
	} 
	  
	def doWikify(context:QLContext)(v:DiscreteSummary[UVT], displayOpt:Option[Wikitext] = None, lexicalThing:Option[PropertyBundle] = None):Future[Wikitext] = {
	  implicit val s = context.state
	  val fromPropOpt = context.state.prop(v.propId)
	  // Iff this is coming from a DiscreteType, with a well-defined range, use that range to determine
	  // everything to wikify:
	  val range = fromPropOpt.flatMap { fromProp =>
	    fromProp.pType match {
	      case dt:DiscreteType[UVT] => Some(dt.range(fromProp.confirmType(dt).get))
	      case _ => None
	    }
	  }
	  val pairs = range match {
	    // TBD: the .reverse below is because we want to display star ratings from best to least in the
	    // histogram. Is that always true? We may need to let subclasses chime in on this.
	    case Some(keys:Seq[UVT]) => keys.map(key => (key, v.content.get(key).getOrElse(0))).reverse
	    case _ => v.content.toSeq
	  }
	  
	  Future.successful((Wikitext("""
	              |!+noLines
	              |<dl class="histogram">""".stripMargin) /: pairs) 
	  { (curText, pair) =>
	    val (key, num) = pair
	    curText + Wikitext("<dt>") + wikifyKey(context, fromPropOpt, key) + Wikitext("</dt><dd>" + num.toString + "</dd>\n")
	  } + Wikitext("</dl>\n!-noLines\n"))
	}
	  
	def doDefault(implicit state:SpaceState):DiscreteSummary[UVT] = DiscreteSummary(UnknownOID, Map())
	
	def renderInputXml(prop:Property[_,_], context:QLContext, currentValue:DisplayPropVal, v:ElemValue):NodeSeq =
	  <p><i>This will be calculated; you don't input it directly.</i></p>
  }  
}
