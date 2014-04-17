package querki.uservalues

import models.{OID, Property, Wikitext}

import querki.values.{QLContext, SpaceState}

/**
 * Describes a mechanism for summarizing the User Values for a Property. The summary then gets
 * stored on the Thing itself.
 * 
 * It is possible that Summarizer should be a Thing -- not sure yet. We can probably handle that
 * as a subclass, if so.
 */
trait Summarizer[UVT,VT] {
  /**
   * Based on the previous and current User Values for a single User, produce an updated summary
   * value for this Thing.
   */
  def addToSummary(tid:OID, prop:Property[VT,_], previous:Option[UVT], current:Option[UVT])(implicit state:SpaceState):VT
  
  /**
   * Deserialize the summary, in whatever way is appropriate.
   */
  def doDeserialize(ser:String)(implicit state:SpaceState):VT
  
  /**
   * Serialize the summary.
   */
  def doSerialize(v:VT)(implicit state:SpaceState):String
  
  /**
   * Display the summary.
   */
  def doWikify(context:QLContext)(v:VT, displayOpt:Option[Wikitext] = None):Wikitext
  
  /**
   * The default value of the summary.
   */
  def doDefault(implicit state:SpaceState):VT
}

/**
 * A trivial Summarizer, for use in Types that don't have a natural Summary.
 */
class EmptySummarizer[UVT] extends Summarizer[UVT, Unit] {
  def addToSummary(tid:OID, prop:Property[Unit,_], previous:Option[UVT], current:Option[UVT])(implicit state:SpaceState):Unit = {}
  def doDeserialize(ser:String)(implicit state:SpaceState):Unit = {}
  def doSerialize(v:Unit)(implicit state:SpaceState):String = ""
  def doWikify(context:QLContext)(v:Unit, displayOpt:Option[Wikitext] = None):Wikitext = Wikitext.empty 
  def doDefault(implicit state:SpaceState):Unit = {}
}
