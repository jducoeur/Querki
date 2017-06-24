package querki.uservalues

import models.{Property, Thing}
import querki.globals._
import querki.spaces._
import querki.spaces.messages.SpacePluginMsg
import querki.util.UnexpectedPublicException

import PersistMessages._

case class RecalculateSummaries[UVT](fromProp:Property[UVT,_], summaryId:OID, values:Seq[OneUserValue])

/**
 * When a UserSpaceSession changes a UserValue, it may send a SummarizeChange message to the Space, telling it to
 * update the Summary for that Property. This handles the message in a plugin, so the Space code doesn't
 * need to know about all that.
 */
class UserValueSpacePlugin[RM[_]](s:SpaceAPI[RM], rtc:RTCAble[RM])(implicit val ecology:Ecology) 
  extends SpacePlugin(s, rtc) with EcologyMember 
{
  lazy val IdentityAccess = interface[querki.identity.IdentityAccess]
  
  // TBD: this doesn't really check the types as well as we'd like it to do, due to erasure. Are all the needed types
  // lost too far upstream? Is there any way to introduce TypeTags to allow us to actually check Summarizer[UVT,VT]?
  def asSummarizer[UVT,MSG[UVT],VT](prop:Property[VT,_], msg:MSG[UVT]):Option[(Property[VT,_], Summarizer[UVT,VT])] = {
    if (prop.pType.isInstanceOf[Summarizer[_,_]])
      Some((prop.asInstanceOf[Property[VT,_]], prop.pType.asInstanceOf[Summarizer[UVT,VT]]))
    else
      None
  }
  
  def summarizeChange(msg:SummarizeChange[_])(state:SpaceState):RM[ChangeResult] = {
    implicit val s = state
    val SummarizeChange(tid, fromProp, summaryId, previous, current) = msg
    val resultOpt = for {
      rawProp <- state.prop(summaryId) orElse QLog.warn(s"UserValueSpacePlugin didn't find requested Summary Property $summaryId")
      thing <- state.anything(tid) orElse QLog.warn(s"UserValueSpacePlugin didn't find requested Thing $tid")
      (summaryProp, summarizer) <- asSummarizer(rawProp, msg)
      newSummary = summarizer.addToSummary(tid, fromProp, summaryProp, previous, current)
      newProps = thing.props + (summaryProp.id -> newSummary)
    }
      yield space.modifyThing(IdentityAccess.SystemUser, tid, None, newProps, true)(state)
      
    resultOpt.getOrElse(rtc.failed(UnexpectedPublicException))
  }
  
  def receive = {
    case SpacePluginMsg(req, _, msg:SummarizeChange[_]) => {
      space.runAndSendResponse("summarizeChange", false, summarizeChange(msg), false)(space.currentState)
    }
    
    case SpacePluginMsg(_, _, msg @ RecalculateSummaries(fromProp, summaryId, values)) => {
      // TBD -- I am *deeply* uncomfortable with the concept of RecalculateSummaries in the new architecture. This
      // should get a rethink after we rewrite User Values in the new Persistence model.
//      implicit val state = space.state
//      for {
//        rawProp <- state.prop(summaryId) orElse QLog.warn(s"UserValueSpacePlugin didn't find requested Summary Property $summaryId")
//        (summaryProp, summarizer) <- asSummarizer(rawProp, msg)
//        // IMPORTANT: note that we'll get one result for each change that is needed. This can produce a
//        // non-trivial number of modifyThing requests:
//        (tid, newSummary) <- summarizer.recalculate(fromProp, summaryProp, values)
//      }
//        space.modifyThing(IdentityAccess.SystemUser, tid, None, ((t:Thing) => t.props + (summaryProp.id -> newSummary)), false)
    }
  }
}
