// Querki Conversations code
//
// This is intended for use with convPane.html, and not much anywhere else. Note that this
// depends on other script files loaded in convPane!

function loadConversations(ownerId, spaceId, thingId, convContainer, canComment) {

  function newConversationInput(placement, container, placeholder, responseToCb, postedCb) {
    var inputArea = $("#_addCommentTemplate").clone(true).attr('id', '_addComment');
    inputArea.show();
    placement.after(inputArea);
    
    var postButton = inputArea.find("._postCommentButton");
    var textArea = inputArea.find("._commentInput");
    textArea.autosize();
    textArea.attr("placeholder", placeholder);
    
    postButton.click(function (evt) {
      var text = textArea.val();
      convJsRoutes.controllers.ConversationController.addComment(ownerId, spaceId, thingId, text, responseToCb()).ajax({
        success: function(result) {
          var parentId = result.parentId;
          var node = result.node;
          insertNode(node, container);
          if (postedCb != null) {
            postedCb();
          }
        },
        error: function(err) {
  	      alert("Error: " + err.responseText);
        }
      });    
      textArea.val("");
    });
    
    return inputArea;
  }
  
  newConversationInput(convContainer, null, "Start a new conversation...", function() { return ""; });
  
  function insertNode(node, existingThread) {
    var comment = node.comment;
    var id = comment.id;
    
    var threadDisplay = existingThread
    var isNewThread = false;
    if (threadDisplay == null) {
      threadDisplay = $("#_convThreadTemplate").clone(true).attr('id', '_convThread' + id);
      threadDisplay.show();
      convContainer.append(threadDisplay);
      isNewThread = true;
    }
    commentContainer = threadDisplay.find("._commentContainer");
    
    function insertComment(c) {
      var author = c.author;
      // Note that, for the date, we use the moment library, which is loaded by convPane:
      var created = moment(c.createTime);
      var createdDisplay = created.calendar();
      var text = c.html;
      var commentDisplay = $("#_commentTemplate").clone(true).attr('id', '_comment' + id)
      commentDisplay.find("._commentLink").prop('name', 'comment' + id);
      commentDisplay.find("._commentAuthor").html(author);
      commentDisplay.find("._commentTime").html(createdDisplay);
      commentDisplay.find("._commentText").html(text);
      commentDisplay.show();
      commentContainer.append(commentDisplay);
    }
    
    insertComment(comment);
    
    if (isNewThread) {
	    var replyPlaceholder = threadDisplay.find("._replyPlaceholder");
	    var realReply = newConversationInput(replyPlaceholder, threadDisplay, "Reply...", 
	      function () {
	        // TODO: this is *not* correct yet! It should actually run down to the end of the
	        // replies, and return the ID of that one. We probably want a data structure to track
	        // the topology of the comments...
	        return id;
	      },
	      function () {
	        // This is called when we successfully post.
	        realReply.hide();
	        replyPlaceholder.show();
	      });
	    realReply.hide();
	    function showRealReply(evt) {
	      realReply.show();
	      replyPlaceholder.hide();
	      realReply.find("._commentInput").focus();    
	    }
	    replyPlaceholder.click(showRealReply);
	    replyPlaceholder.keydown(showRealReply);
	    realReply.blur(function (evt) {
	      replyPlaceholder.show();
	      realReply.hide();
	    });
    }
    
    function addResponses(n) {
      var responses = n.responses;
      _.each(responses, function(responseNode) {
        var responseComment = responseNode.comment;
        if (responseComment.primary) {
          insertComment(responseComment);
          addResponses(responseNode);
        } else {
          // TODO: deal with non-primaries, which need a nested thread:
          console.log("Non-primary response -- id:" + responseComment.id + " text: " + responseComment.html + " primary: " + responseComment.primary);      
        }
      });
    }
    addResponses(node);
  }

  convJsRoutes.controllers.ConversationController.getConversations(ownerId, spaceId, thingId).ajax({
    success: function (convs) {
      _.each(convs, function(node) { insertNode(node, null); });
	},
	error: function (err) {
	  alert("Error: " + err.responseText);
	}
  });
}