        
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
          var myId = this.prop('name');
	      var prop = this.data("propid");
	      var thingId = this.data("thing");
          var name = myId + "_values";
          var values = this.manifest('values');
          var wrapped = {};
          wrapped[name] = values;
          var serialized = $.param(wrapped);          
          console.log(myId + " as an array is " + serialized);
          
          // TODO: this ought to be merged with the similar code in thing.scala.html, but note that this
          // comes by the serialized form very differently:
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
	  });
    });
  }
        
}( jQuery ));

$(function() {
});