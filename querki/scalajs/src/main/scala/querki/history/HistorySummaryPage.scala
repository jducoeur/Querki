package querki.history

import scala.scalajs.js

import scalatags.JsDom.all._
import autowire._

import org.widok.moment._

import org.querki.gadgets.core.SimpleGadget
import org.querki.jquery._

import models.Kind
import querki.display.{ButtonGadget, Dialog}
import querki.globals._
import querki.pages._
import querki.time._
import querki.time.Common.Timestamp

import HistoryFunctions._

class HistorySummaryPage(params:ParamMap)(implicit val ecology:Ecology) 
  extends Page("historySummary") with querki.util.ScalatagUtils
{
  lazy val Client = interface[querki.client.Client]
  lazy val DataSetting = interface[querki.data.DataSetting]
  lazy val History = interface[History]
  
  lazy val spaceInfo = DataAccess.space.get
  
  lazy val setStateMsgs = Localization.messages("history").getPackage("setStateReasons")
  
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
      case SetStateSummary(idx, who, time, reason, details) => {
        val msg = setStateMsgs.msg(reason.msgName, ("details" -> details))
        SummaryDisplayInfo(idx, who, time, msg, "", "success")
      }
      case ImportSummary(idx, who, time) => 
        SummaryDisplayInfo(idx, who, time, s"Imported/converted Space '${spaceInfo.displayName}'", "", "success")
      case CreateSummary(idx, who, time, kind, id, model) => 
        SummaryDisplayInfo(idx, who, time, s"Created ${showType(kind)}${lookup(id)}", "", "success")
      case ModifySummary(idx, who, time, id, props) => 
        SummaryDisplayInfo(idx, who, time, s"Edited ${lookup(id)}", s"Changed: ${props.map(lookup(_)).mkString(", ")}", "info")
      case DeleteSummary(idx, who, time, id) =>
        SummaryDisplayInfo(idx, who, time, s"Deleted ${lookup(id)}", "", "danger")
      case AddAppSummary(idx, who, time, appId) =>
        SummaryDisplayInfo(idx, who, time, s"Added app ${lookup(appId)}", "", "info")
    }
  }
  
  def rollbackTo(info:SummaryDisplayInfo) = {
    val confirmDialog = new Dialog("Confirm Revert",
      div(
        s"""This will revert the state of this Space back to version ${info.idx}, dated ${displayTime(info.time)}. It will not
           |lose any data, but any changes made since then will be hidden. You can undo this change by reverting again, to
           |the most recent previous event. Are you sure you want to do this?""".stripMargin
      ),
      (ButtonGadget.Warning, Seq("Revert", id:="_confirmRevert"), { dialog =>
        Client[HistoryFunctions].rollbackTo(info.idx).call().map { reloadedSpaceInfo =>
          dialog.done()
          DataSetting.setSpace(Some(reloadedSpaceInfo))
          PageManager.reload()
        }
      }),
      (ButtonGadget.Normal, Seq("Cancel", id:="_cancelRevert"), { dialog =>
        dialog.done()
      })
    )
    
    confirmDialog.show()
  }
  
  def evtDisplay(summary:EvtSummary, whoMap:IdentityMap, thingNames:ThingNames) = {
    val info = getEvtDisplayInfo(summary, whoMap, thingNames)
    
    val whoName = whoMap.get(info.who).map(_.name).getOrElse("")
    
    new SimpleGadget(tr(
      data("idx") := info.idx,
      cls:=info.colorClass,
      td(displayTime(info.time)),
      td(whoName),
      td(raw(info.title)),
      td(info.details)
    ), { e =>
      $(e).click { evt:JQueryEventObject =>
        val isOpen = $(e).data("isopen").map(_.asInstanceOf[Boolean]).getOrElse(false)
        if (isOpen) {
          val details = $(e).next()
          details.hide("fast", { () => details.remove() })
          $(e).data("isopen", false)
        } else {
          val sib = 
            tr(
              cls:=info.colorClass,
              display:="none",
              td(
                colspan:=1,
                new ButtonGadget(ButtonGadget.Info, "View Space") ({ () =>
                  History.setHistoryVersion(info.idx, info.time)
                  Pages.showSpacePage(spaceInfo)
                })
              ),
              td(
                colspan:=2,
                new ButtonGadget(ButtonGadget.Warning, "Revert to Here") ({ () =>
                  rollbackTo(info)
                })
              ),
              td(
                colspan:=1,
                " "
              )
            ).render
          $(e).after(sib)
          $(sib).show("fast")
          $(e).data("isopen", true)
        }
      }
    })
  }
  
  def pageContent = for {
    summary <- Client[HistoryFunctions].getHistorySummary().call()
    HistorySummary(evts, EvtContext(whoMap, thingNames)) = summary
    guts = 
      div(
        h3(s"History of ${spaceInfo.displayName} ",
          Gadget(querkiButton("Done"), { e => 
            $(e).click({ evt:JQueryEventObject => Pages.showSpacePage(spaceInfo) })
          }),
          
          " ",
          
          Gadget(iconButton("refresh")(title:="Refresh this page"), { e => 
            $(e).click({ evt:JQueryEventObject => PageManager.reload() }) 
          })
        ),
          
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
