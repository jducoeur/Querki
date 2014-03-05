// ------------------------
//
// Common Querki Javascript
//
// This is where all sorts of utility code should eventually live.
//
// IMPORTANT: remember that this file is *not* processed by Play, so it can't access the Play variables
// directly. Those need to come out of main.scala.html or other files that are Play-processed.
//
// ------------------------

// This is an ugly global variable, but it's a useful state switch of whether to
// allow things to live-update or not.
var querkiLiveUpdate = true;
        
function showStatus(msg) {
  $("#statusText").text(msg);
  $("#statusLine").show();
}
        
function finishStatus(msg) {
  $("#statusText").text(msg);
  $("#statusLine").show().delay(4000).hide("slow");
}

// JQuery plugin for Manifest-izing a control
(function( $ ) {

  $.fn.asManifest = function (ownerId, spaceId) {
    this.each(function () {
      // We don't want to Manifest-ize stuff in a template, because that gets in the way of proper
      // initialization later, when we instantiate that template:
      if ($(this).parents(".inputTemplate").length > 0) {
        return;
      }
    
      var propId = $(this).data('prop');
      var isNames = $(this).data('isnames');
      var current = $(this).data('current');
            
      var entryPoint = 'getLinks';
      var required = true;
      var typeName = 'thing';
      if (isNames) { 
        entryPoint = 'getTags';
        required = false;
        typeName = 'tag';
      }
            
      $(this).manifest({
        marcoPolo: {
		  url: entryPoint + '?propId=' + propId,
		  minChars: 1,
		  required: required,
		  formatData: function(data) { return data; },
		  formatItem: function(data, $item) { return data.display; },
		  formatNoResults: function(q, $item) { return "No existing " + typeName + " with <b>" + q + "</b> found."; }
		},
        formatDisplay: function (data, $item, $mpItem) {
          if (_.isString(data))
            return data;
          else 
            return data.display; 
        },
		formatValue: function(data, $value, $item, $mpItem) {
		  if (_.isString(data))
		    return data;
		  else 
		    return data.id; 
		},
		// Use Enter as the separator to create new items, as well as the default comma:
	    separator: [13, ','],
        values: current,
        required: required,
        onChange: function(type, data, $item) {
          if (querkiLiveUpdate) {
            var myId = this.prop('name');
	        var prop = this.data("propid");
	        var thingId = this.data("thing");
            var name = myId + "_values";
            var values = this.manifest('values');
            var wrapped = {};
            wrapped[name] = values;
            var serialized = $.param(wrapped);          
          
            // TODO: this ought to be merged with the similar code in thing.scala.html, but note that this
            // comes by the serialized form very differently. The problem is that "this" isn't what we
            // need to serialize; instead, the form is composed of a bunch of hidden fields with names
            // ending with "_values". Note that we also precede this with an *empty* copy of the property,
            // to clear it before we set the current values -- otherwise, deletions don't always take.
	        jsRoutes.controllers.Application.setProperty2(ownerId, spaceId, thingId).ajax({
	          data: "addedProperty=&model=&" + myId + "=&" + serialized,
	          success: function (result) {
	            finishStatus("Saved");
	          },
	          error: function (err) {
	            showStatus("Error trying to save. Please reload this page and try again.");
	          }
	        });
	        showStatus("Saving...");
	      }
        }
	  });
    });
  }
        
}( jQuery ));

// **********************************************
//
// jQuery plugin for re-arrangeable Lists
//

(function( $ ) {

    function setButton(t, icon, title, cb) {
	  t.html("<i class='" + icon + "'></i>");
	  t.addClass("btn");
      t.attr("title", title);
      t.off("click");
      t.on("click", cb);          
    }
        
    function dataField(t, fieldName) {
      return "#" + t.data(fieldName);
    }
        
    function viaField(t, fieldName) {
      var controlId = dataField(t, fieldName);
      return $(controlId);
    }
    
    function eventWithCallback(func, cb) {
      return function(evt) {
        return func(evt, cb);
      }
    }
    
    $.fn.addListItemButton = function (cb) {
      this.each(function () {
        setButton($(this), "icon-plus-sign", "Add Item", eventWithCallback(handleAddListItem, cb));
      });
    }
    
    $.fn.deleteListItemButton = function (cb) {
      this.each(function () {
        setButton($(this), "icon-remove-sign", "Delete Item", eventWithCallback(handleDeleteListItem, cb));
      });
    }
    
    function replaceIndexes(newField, curSize) {
      var replacement = "[" + curSize + "]";
      newField.find(".propEditor").each(function () {
        var propid = $(this).data("propid");
        if (typeof(propid) != "undefined") {
          propid = propid.replace("[-1]", replacement);
          $(this).data("propid", propid)
        }
        var name = $(this).attr("name");
        if (typeof(name) != "undefined") {
          name = name.replace("[-1]", replacement);
          $(this).attr("name", name)
        }
      });
    }
    
    function handleAddListItem(evt, cb) {
      var target = $(evt.currentTarget);
      var templateField = target.parent().find(".inputTemplate").first();
      var list = target.parent().find("ul").first();
      var sizeField = viaField(target, "size");
      var curSize = Number(sizeField.val());
      var newField = templateField.clone(true);
      newField.attr("name", templateField.data("basename") + "[" + curSize + "]");
      newField.removeClass("inputTemplate");
      newField.show();
      replaceIndexes(newField, curSize);
      var newLi = $("<li><span class=\"icon-move\"></span></li>");
      newLi.append(newField);
      var delButton = $("<button class=\"delete-item-button btn-mini\">&nbsp;</button>");
      setButton(delButton, "icon-remove-sign", "Delete Item", handleDeleteListItem);
      newLi.append(delButton);
      list.append(newLi);
      sizeField.val(curSize + 1);
      // Do our best to set focus to the first relevant field of the new element:
      newLi.find(".propEditor,input,textarea").first().focus();
      cb(list.parent(), newLi);
      return false;
    }
    
    function handleDeleteListItem(evt, cb) {
      var targetLi = $(evt.currentTarget).parent();
      var sortList = targetLi.parent();
      targetLi.detach();
      renumberList(sortList);
      cb(sortList.parent());
      return false;
    }
    
}( jQuery ));
      
function renumberList(sortList) {
  var allItems = sortList.children("li");
  var templateField = sortList.parent().find(".inputTemplate").first();
  var baseItemName = templateField.data("basename");
  var i = 0;
  allItems.each(function () {
    var listItem = $(this);
    var inputField = listItem.find(".list-input-element");
    inputField.attr("name", baseItemName + "[" + i + "]");
    i = i + 1;
  });        
}
    
function onSortFinished(evt, ui) {
  var itemMoved = ui.item;
  var sortList = itemMoved.parent();
  renumberList(sortList);
  return sortList;
}       

// **********************************************
//
// Supporting code for Create from Link:
//

var selectCreatingNew; 
function handleLinkSelectChanged(evt) {
  selectCreatingNew = this;
  var optionSelected = $("option:selected", this);
  if (optionSelected.hasClass("_createNewFromModel")) {
    var modelId = optionSelected.data("model");
    // Note that createThingRoute is defined in main.html:
    $("#create-instance-iframe").attr("src", createThingRoute() + "?cl&subCreate=true&model=" + modelId)
    $("#create-instance-dialog").dialog("open");
	return false;          
  } 
}
  
function onLinkCreated(newThingId, newDisplayName) {
  $(selectCreatingNew).append('<option value="' + newThingId + '" selected="selected">' + newDisplayName + '</option>');
  $("#create-instance-dialog").dialog("close");
  $(selectCreatingNew).closest(".propEditor").each(function () { 
    editControlChanged($(this)); 
  })
}

function setupCreateFromLink(root) {
  // TODO: figure out how to make this nicely Bootstrappy and responsive:
  var wWidth = $(window).width();
  var dWidth = wWidth * 0.9;
  var wHeight = $(window).height();
  var dHeight = wHeight * 0.9;
  $("#create-instance-dialog").dialog({
	  autoOpen:false,
	  height: dHeight,
	  width: dWidth,
	  modal: true,
	  buttons: {
	      "Cancel": function () {
		    $(this).dialog("close");
		  }
	  },
	  close: function () {
	  }
  });
  
  root.find("._linkSelect").change(handleLinkSelectChanged);
}

// Scroll to the bottom of the window, with a reasonably pleasant animation:
function animateScrollToBottom() {
  $('html, body').animate({ 
     scrollTop: $(document).height()-$(window).height()}, 
     500, 
     "swing"
  );
}

function instantScrollToBottom() {
  $('html, body').scrollTop($(document).height()-$(window).height());
}

// **********************************************

var updateCB;
function editControlChanged(target) {
  updateCB(target);
}

function finalSetup(ownerId, spaceId, root) {

  function insertThingEditor(thingId, beforeNode) {
    jsRoutes.controllers.Application.getThingEditor(ownerId, spaceId, thingId).ajax({
      success: function (result) {
        var newElem = $(result);
	    newElem.insertBefore(beforeNode);
        finalSetup(ownerId, spaceId, newElem);
        instantScrollToBottom();
        newElem.find(".propEditor").first().focus();
      },
      error: function (err) {
        showStatus("Couldn't fetch the editor -- refresh this page!");
      }
    });
  }

  function createAnotherThing(evt) {
    var modelId = $(this).data("model");
    var createButton = $(this);
    jsRoutes.controllers.Application.doCreateThing(ownerId, spaceId).ajax({
      data: "API=true&model=" + modelId,
      success: function (result) {
	    insertThingEditor(result, createButton);
	  },
	  error: function (err) {
	    alert("Error: " + err.responseText);
	  }
    });
  }
  root.find("._createAnother").click(createAnotherThing);
  
  function reallyDelete(evt) {
    var deleteButton = $(this);
    var editor = deleteButton.parents("._instanceEditor");
    var thingId = editor.data("thingid");
    jsRoutes.controllers.Application.deleteThing(ownerId, spaceId, thingId).ajax({
      data: "API=true",
      success: function(result) {
        editor.hide(400, function () { editor.remove(); });
      },
      error: function (err) {
        showStatus("Error: " + err);
      }
    });  
  }
  
  // Confirmation: we show a popover, and give the user a couple of seconds to press delete
  // again to really do it. Otherwise, auto-cancel.
  function deleteInstance(evt) {
    var deleteButton = $(this);
    deleteButton.popover({
      content: "Click again to delete",
      placement: 'left',
      trigger: 'manual'
    });
    deleteButton.popover('show');
    deleteButton.off('click', null, deleteInstance);
    deleteButton.on('click', null, reallyDelete);
    var timeout = setTimeout(
      function () {
        deleteButton.popover('hide');
        deleteButton.off('click', null, reallyDelete);
        deleteButton.on('click', null, deleteInstance);
      },
      2000
    );
  }
  root.on('click', "._deleteInstanceButton", deleteInstance)
  
  function renumberModelList(target) {
    var prop = target.data("propid");
    var thingId = target.data("thing").substring(1);
    var targetRegex = new RegExp(prop + "\\[" + "\\d" + "\\]", "g");
    var i = 0;
    target.find(".list-input-element").each(function () {
      var element = $(this);
      var replacement = prop + "[" + i + "]";
      element.find(".propEditor").each(function () {
        var editor = $(this);
        var propid = editor.data("propid");
        if (typeof(propid) != "undefined") {
          propid = propid.replace(targetRegex, replacement);
          $(this).data("propid", propid)
        }
        var name = $(this).attr("name");
        if (typeof(name) != "undefined") {
          name = name.replace(targetRegex, replacement);
          $(this).attr("name", name)
        }        
      });
      i = i + 1;
    });
  }

  function doUpdateValue(target, successCb, failureCb) {
      var prop = target.data("propid");
      var thingId = target.data("thing");
      var serialized = "";
      if (target.hasClass("modelValue")) {
        // For the moment, we are sending the entire list every time a major change event
        // occurs. At some point, we must change this to work with diffs instead.
        renumberModelList(target);
        var childHolder = target;
        if (target.hasClass("coll-list-input")) {
          // To make sure that we don't send the input template as well:
          childHolder = target.find(".sortableList").first();
        }
        // Clear the existing value, since we're going to send everything:
        serialized = serialized + "&" + target.attr("name") + "=";
        childHolder.find(".propEditor").each(function () {
          var editControl = $(this);
          serialized = serialized + "&" + editControl.serialize();
        });
      } else {
        if (target.hasClass("radioBtn")) {
          serialized = target.prop("name") + "=" + target.prop("value");
        } else if (target.hasClass("coll-list-input")) {
          // Serialize each of the elements of this list:
          serialized = target.find(".list-input-element").serialize();
        } else {
          serialized = target.serialize();
        }
        serialized = "&" + serialized
      }
      console.log("Serialized value of " + thingId + ":" + prop + " is " + serialized);
      jsRoutes.controllers.Application.setProperty2(ownerId, spaceId, thingId).ajax({
        data: "addedProperty=&model=" + serialized,
        success: function (result) {
          finishStatus("Saved");
          if (typeof(successCb) != "undefined") {
            successCb();
          }
        },
        error: function (err) {
          showStatus("Error trying to save. Please reload this page and try again.");
        }
      });
      showStatus("Saving...");
  }
  
  function updateIfLive(target) {
    if (querkiLiveUpdate) {
      doUpdateValue(target);
    }  
  }
  updateCB = updateIfLive;
  
  function updateValue(evt) {
    updateIfLive($(this));
  }

  // For now, we're specifically omitting selects inside list editors, because they aren't rendering right.
  // TODO: Fix this!
  // TODO: SelectBoxIt is nice in principle, but I keep hitting annoying edge cases. So disabling it for now.
  //$("select").filter(":not(.sortableList select)").selectBoxIt();
  
  function onListItemAdded(listRoot, newItem) {
    finalSetup(ownerId, spaceId, newItem);
    updateIfLive(listRoot);
  }
  
  // Invoke the List mechanisms, if appropriate:
  root.find(".add-item-button").addListItemButton(onListItemAdded);
  root.find(".delete-item-button").deleteListItemButton(updateIfLive);
  root.find(".sortableList").sortable({
    stop:function(evt, ui) {
      // onSortFinished() returns the sortableList itself...
      var sortList = onSortFinished(evt, ui);
      // ... from which we perform the update on the coll-list-input, which is its parent:
      updateIfLive(sortList.parent());
    }
  });      
  root.find(".inputTemplate").hide();
  
  root.find("._withTooltip").tooltip({ delay: 250 });
  
  root.find("._tagSetInput").asManifest(ownerId, spaceId);
  
  root.find(".controls ._largeTextEdit").addClass("span10");
  root.find("._largeTextEdit").autosize();
  root.find(".controls input[type='text']").filter(".propEditor").addClass("span10");

  root.find(".propEditor").change(updateValue);
  root.find(".propEditor .radioBtn").click(updateValue);
  
  setupCreateFromLink(root);
  
  // --------------------------
  // Pick List Management
  //
  // TODO: this code is horribly incestuous with HtmlRenderer. How can we modularize all of
  // this stuff more appropriately?
  // TODO: all of this should get replaced by the same mechanism we use for Add Instance.
  function addToList(target, newId) {
    var master = target.parents(".propEditor");
    var basename = master.prop("name");
    var currentElems = master.find("ul li");
    var currentNum = currentElems.length;
    // TODO: ack, horror -- newId, returned from doCreateThing, is a ThingId, but the input field needs a plain OID.
    // Need to iron this inconsistency out, and choose the paradigm for which you use when.
    var thingId = newId.substring(1);
    
    master.append("<input type='hidden' name='" + basename + "_values[" + currentNum + "]' value='" + thingId + "'></input>");
    doUpdateValue(master, function () { location.reload(true); });
  }
  
  // Note that quickCreate is intentionally crafted to be somewhat reusable:
  function doQuickCreate(target) {
    var modelId = target.data("model");
    var prop = target.data("propid");
    var createVal = target.val();
    var addtolist = target.data("addtolist");
    jsRoutes.controllers.Application.doCreateThing(ownerId, spaceId).ajax({
      data: "API=true&model=" + modelId + "&v-" + prop + "-=" + createVal,
      success: function (result) {
        finishStatus("Added");
        // TODO: this is horrible coupling. I'd really like to keep quickCreate and addToList separate.
        // How can we get that to work in the JS environment?
        if (addtolist) {
          addToList(target, result);
        }
      },
      error: function (err) {
        showStatus("Error trying to add: " + err);
      }
    });
    showStatus("Creating...");
  }
  function quickCreate(evt) {
    var button = $(this);
    var target = button.siblings("._quickCreateProp");
    doQuickCreate(target);
    evt.stopPropagation();  
  }
  root.find("._quickCreate").click(quickCreate);
  root.find("._quickCreateProp").change(function(evt) { evt.stopPropagation(); });
  root.find("._quickCreateProp").keydown(function (e) {
    var keyCode = e.keyCode || e.which;

    if (keyCode == 13) {
      doQuickCreate($(this));
      return false;
    }
  });
  // ----------------------------
}
