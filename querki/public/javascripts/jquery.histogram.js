/*!
 *
 * jQuery plugin to display a numeric dlist as a simple histogram
 *
 * Requires both jQuery and Underscore.
 *
 * Usage:
 *
 *   <dl class="myHistogram">
 *     <dt>foo</dt><dd>42</dd>
 *     <dt>bar</dt><dd>12</dd>
 *   </dl>
 *
 *   $('.myHistogram').histogram();
 *
 * This plugin is licensed under the MIT license, and may be freely redistributed.
 */
 
(function( $ ) {

  $.fn.histogram = function() {
    this.each(transform);    
  }
  
  function transform() {
    // The initial dlist that we are transforming:
    var dl = $(this);
    
    // Parse out the received information:
    var maxwidth = 100;  // TODO: can we take this from the width or something?
    var labels = dl.find("dt").map(function () { 
      var dt = $(this);
      return dt.text(); 
    }).toArray();
    var scores = dl.find("dd").map(function () { 
      return parseInt($(this).text()); 
    }).toArray();
    var maxscore = _.max(scores);
    var scale = maxwidth / maxscore;
    
    // Create the histogram:
    var pairs = _.zip(labels, scores);
    var table = $("<table class='generatedHistogram'></table>");
    _.each(pairs, function (pair, index) {
      table.append($("<tr><td class='histoName'>" + pair[0] + 
         "</td><td class='histoScore'>" + pair[1] + 
         "</td><td class='histoBar histoBar-" + index + "' style='width:" + (pair[1] * scale) + "px;'></td></tr>"));
    });
    dl.after(table);
    
    // Finally, hide the original dlist:
    dl.hide();
  }
        
}( jQuery ));
