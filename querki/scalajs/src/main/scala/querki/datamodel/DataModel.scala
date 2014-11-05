package querki.datamodel

import scalatags.JsDom.all._

import querki.globals._

import querki.data.ThingInfo
import querki.display.Dialog

class DataModelEcot(e:Ecology) extends ClientEcot(e) with DataModel {
  
  def implements = Set(classOf[DataModel])
/*
			@if(thing.isDefined) {
				$("#deleteThing").on("click", function(evt) {
				  $("#delete-dialog").dialog("open");
				  return false;
				});
			
				$("#delete-dialog").dialog({
				  autoOpen:false,
				  height: 200,
				  width: 350,
				  modal: true,
				  buttons: {
				    "Delete": function () {
		              window.location = "@routes.Application.deleteThing(rc.ownerHandle, space.get.toThingId, thing.get.toThingId)";
				    },
				    "Cancel": function () {
				      $(this).dialog("close");
				    }
				  },
				  close: function () {
				  }
				});		
			}
	@if(thing.isDefined) {
      <div id="delete-dialog" title="Confirm Delete" style="display:none">
        <p><b>Are you sure you want to delete @Html(thing.get.displayName)? There is currently no way to get it back!</b></p>
      </div>	
	}
 */
  
  def deleteAfterConfirm(thing:ThingInfo) = {
    val deleteDialog:Dialog = 
      new Dialog("Confirm Delete", 200, 350,
        p(b(s"Are you sure you want to delete ${thing.displayName}? There is currently no way to get it back!")),
        ("Delete" -> { dialog => println(s"I'm about to delete ${thing.displayName}"); dialog.done() }),
        ("Cancel" -> { dialog => dialog.done() })
      )
    println(s"About to show dialog to delete $thing")
    deleteDialog.show()
  }
}
