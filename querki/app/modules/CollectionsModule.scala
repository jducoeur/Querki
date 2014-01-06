package modules.collections

import models._
import models.system._
import ql._

import querki.ecology._

import querki.values._

import Thing._

object MOIDs extends EcotIds(6) {
  val PrevInListOID = moid(1)
  val NextInListOID = moid(2)
}

class CollectionsModule(e:Ecology, val moduleId:Short) extends modules.Module(e) {
  
  import MOIDs._

  /***********************************************
   * PROPERTIES
   ***********************************************/
  
  lazy val prevInListMethod = new InternalMethod(PrevInListOID,
      toProps(
        setName("_prevInList"),
        Summary("Fetch the previous value to this one from the given List"),
        Details("""    THING -> _prevInList(LIST) -> PREVIOUS THING 
        		|Given a THING, and a LIST that contains that THING, this returns the *previous* THING to that
        		|in the LIST. It returns None iff the THING is not in the LIST, or if it is the beginning of the LIST.""".stripMargin)))
  {
    override def qlApply(context:QLContext, paramsOpt:Option[Seq[QLPhrase]] = None):QValue = {
      paramsOpt match {
        case Some(params) if (params.length > 0) => {
          val thing = context.value.first
          val list = context.parser.get.processPhrase(params(0).ops, context).value
          val index = list.indexOf(thing)
          index match {
            case Some(i) => {
              if (i == 0)
                EmptyValue(thing.pType)
              else
                ExactlyOne(list.elemAt(i - 1))
            }
            case None => EmptyValue(thing.pType)
          }
        }
        case _ => WarningValue("_prevInList requires a List parameter")
      }
    }
  }

  lazy val nextInListMethod = new InternalMethod(NextInListOID,
      toProps(
        setName("_nextInList"),
        Summary("Fetch the next value to this one from the given List"),
        Details("""    THING -> _nextInList(LIST) -> NEXT THING 
        		|Given a THING, and a LIST that contains that THING, this returns the *next* THING to that
        		|in the LIST. It returns None iff the THING is not in the LIST, or if it is the end of the LIST.""".stripMargin)))
  {
    override def qlApply(context:QLContext, paramsOpt:Option[Seq[QLPhrase]] = None):QValue = {
      paramsOpt match {
        case Some(params) if (params.length > 0) => {
          val thing = context.value.first
          val list = context.parser.get.processPhrase(params(0).ops, context).value
          val index = list.indexOf(thing)
          index match {
            case Some(i) => {
              if (i == (list.size - 1))
                EmptyValue(thing.pType)
              else
                ExactlyOne(list.elemAt(i + 1))
            }
            case None => EmptyValue(thing.pType)
          }
        }
        case _ => WarningValue("_nextInList requires a List parameter")
      }
    }
  }

  override lazy val props = Seq(
    prevInListMethod,
    nextInListMethod
  )
  
}