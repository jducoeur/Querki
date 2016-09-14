package querki.history

import scalatags.JsDom.all._
import autowire._

import org.widok.moment._

import models.Kind
import querki.globals._
import querki.pages._
import querki.time._
import querki.time.Common.Timestamp

import HistoryFunctions._

class HistorySummaryPage(params:ParamMap)(implicit e:Ecology) 
  extends Page(e, "historySummary") with EcologyMember with querki.util.ScalatagUtils
{
  lazy val Client = interface[querki.client.Client]
  
  lazy val spaceInfo = DataAccess.space.get
  
  case class SummaryDisplayInfo(idx:Long, who:String, time:Timestamp, title:String, details:String, colorClass:String)
  
  def getEvtDisplayInfo(summary:EvtSummary, whoMap:IdentityMap, thingNames:ThingNames):SummaryDisplayInfo = {
    def lookup(id:TID):String = {
      thingNames.get(id).map(name => s"'$name'").getOrElse("") 
    }
    
    def showType(kind:Kind.Kind):String = {
      if (kind == Kind.Thing)
        ""
      else
        Kind.getName(kind).get + " "
    }
    
    summary match {
      case BootSummary(idx, who, time) => 
        SummaryDisplayInfo(idx, who, time, s"Created Space '${spaceInfo.displayName}'", "", "success")
      case ImportSummary(idx, who, time) => 
        SummaryDisplayInfo(idx, who, time, s"Imported/converted Space '${spaceInfo.displayName}'", "", "success")
      case CreateSummary(idx, who, time, kind, id, model) => 
        SummaryDisplayInfo(idx, who, time, s"Created ${showType(kind)}${lookup(id)}", "", "success")
      case ModifySummary(idx, who, time, id, props) => 
        SummaryDisplayInfo(idx, who, time, s"Edited ${lookup(id)}", s"Changed: ${props.map(lookup(_)).mkString(", ")}", "info")
      case DeleteSummary(idx, who, time, id) =>
        SummaryDisplayInfo(idx, who, time, s"Deleted ${lookup(id)}", "", "danger")
    }
  }
  
  def evtDisplay(summary:EvtSummary, whoMap:IdentityMap, thingNames:ThingNames) = {
    val info = getEvtDisplayInfo(summary, whoMap, thingNames)
    
    val whoName = whoMap.get(info.who).map(_.name).getOrElse("")
    
    tr(
      data("idx") := info.idx,
      cls:=info.colorClass,
      td(displayTime(info.time)),
      td(whoName),
      td(raw(info.title)),
      td(info.details)
    )
  }
  
  def pageContent = for {
    summary <- Client[HistoryFunctions].getHistorySummary().call()
    HistorySummary(evts, EvtContext(whoMap, thingNames)) = summary
    guts = 
      div(
        h3(s"History of ${spaceInfo.displayName}"),
          
        p(s"""The most recent events are on top. Click on an event to see more information and options.""".stripMargin),
             
        table(
          cls:="table table-hover",
          
          tbody(
            for (evt <- evts.reverse)
              yield evtDisplay(evt, whoMap, thingNames)
          )
        )
      )
  }
    yield PageContents(pageTitle, guts)
}
