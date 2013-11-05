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

(function( $ ) {

  $.fn.asManifest = function (ownerId, spaceId) {
    this.each(function () {
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
		  minChars: 3,
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
            // ending with "_values".
	        jsRoutes.controllers.Application.setProperty2(ownerId, spaceId, thingId).ajax({
	          data: "addedProperty=&model=&field[0]=" + prop + "&" + serialized,
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

function finalSetup(ownerId, spaceId, root) {
  // For now, we're specifically omitting selects inside list editors, because they aren't rendering right.
  // TODO: Fix this!
  // TODO: SelectBoxIt is nice in principle, but I keep hitting annoying edge cases. So disabling it for now.
  //$("select").filter(":not(.sortableList select)").selectBoxIt();
  
  root.find("._withTooltip").tooltip({ delay: 250 });
  
  root.find("._tagSetInput").asManifest(ownerId, spaceId);
  root.find(".controls ._largeTextEdit").addClass("span10");
  root.find("._largeTextEdit").autosize();
  root.find(".mf_container").addClass("span10");
  root.find(".controls input[type='text']").filter(".propEditor").addClass("span10");

  function doUpdateValue(target, successCb, failureCb) {
      var prop = target.data("propid");
      var thingId = target.data("thing");
      var serialized;
      if (target.hasClass("radioBtn")) {
        serialized = target.prop("name") + "=" + target.prop("value");
      } else {
        serialized = target.serialize();
      }
      console.log("Serialized value is " + serialized);
      jsRoutes.controllers.Application.setProperty2(ownerId, spaceId, thingId).ajax({
        data: "addedProperty=&model=&field[0]=" + prop + "&" + serialized,
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
  
  function updateValue(evt) {
    if (querkiLiveUpdate) {
      var target = $(this);
      doUpdateValue(target);
    }
  }

  root.find(".propEditor").change(updateValue);
  root.find(".propEditor .radioBtn").click(updateValue);
  
  // --------------------------
  // Pick List Management
  //
  // TODO: this code is horribly incestuous with HtmlRenderer. How can we modularize all of
  // this stuff more appropriately?
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
      data: "API=true&model=" + modelId + "&field[0]=" + prop + "&v-" + prop + "-=" + createVal,
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
