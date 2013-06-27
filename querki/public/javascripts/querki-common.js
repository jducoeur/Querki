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

function finalSetup(ownerId, spaceId) {			
  $("._tagSetInput").asManifest(ownerId, spaceId);
  $(".controls ._largeTextEdit").addClass("span10");
  $("._largeTextEdit").autosize();
  $(".controls .mf_container").addClass("span10");
  $(".controls input[type='text']").filter(".propEditor").addClass("span10");
}
