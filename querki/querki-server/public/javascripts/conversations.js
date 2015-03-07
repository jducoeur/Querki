// Querki Conversations code
//
// This is intended for use with convPane.html, and not much anywhere else. Note that this
// depends on other script files loaded in convPane!

function loadConversations(ownerId, spaceId, thingId, convContainer, canComment) {

  function newConversationInput(placement, container, placeholder, responseToCb, postedCb) {
    if (!canComment) {
      return null;
    }
    
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
  
  // TODO: the next two functions are near-copies of reallyDelete() and deleteInstance(). They should be refactored
  // together! Only reason I'm not doing so yet is because most of this code is likely to go away soon...
  function reallyDeleteComment(evt) {
    var deleteButton = $(this);
    var comment = deleteButton.parents("._comment");
    var commentId = comment.data("commentId");
    convJsRoutes.controllers.ConversationController.deleteComment(ownerId, spaceId, thingId, commentId).ajax({
      data: "API=true",
      success: function(result) {
        comment.hide(400, function () { comment.remove(); });
      },
      error: function (err) {
        showStatus("Error: " + err);
      }
    });  
  }
  
  // Confirmation: we show a popover, and give the user a couple of seconds to press delete
  // again to really do it. Otherwise, auto-cancel.
  function deleteComment(evt) {
    var deleteButton = $(this);
    deleteButton.popover({
      content: "Click again to delete",
      placement: 'left',
      trigger: 'manual'
    });
    deleteButton.popover('show');
    deleteButton.off('click', null, deleteComment);
    deleteButton.on('click', null, reallyDeleteComment);
    var timeout = setTimeout(
      function () {
        deleteButton.popover('hide');
        deleteButton.off('click', null, reallyDeleteComment);
        deleteButton.on('click', null, deleteComment);
      },
      2000
    );
  }
    
  function insertNode(node, existingThread) {
    var comment = node.comment;
    // Note that this is the ID of the head node!
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
      if (c.isDeleted) {
        // TBD: display something here to indicate there is a deleted comment? Certainly need to
        // do so if we are going to allow undelete.
        return;
      }
      
      var commentId = c.id;
      var author = c.author;
      // Note that, for the date, we use the moment library, which is loaded by convPane:
      var created = moment(c.createTime);
      var createdDisplay = created.calendar();
      var text = c.html;
      var commentDisplay = $("#_commentTemplate").clone(true).attr('id', '_comment' + commentId);
      commentDisplay.data("commentId", commentId);
      var commentName = 'comment' + commentId;
      commentDisplay.find("._commentLink").prop('name', commentName);
      commentDisplay.find("._commentAuthor").html(author);
      commentDisplay.find("._commentTime").html(createdDisplay);
      commentDisplay.find("._commentText").html(text);
      if (!c.canDelete) {
        // If the current viewer can't delete this comment, don't show the button:
        commentDisplay.find("._deleteCommentButton").remove();
      } else {
        commentDisplay.on('click', "._deleteCommentButton", deleteComment)
      }
      commentDisplay.show();
      commentContainer.append(commentDisplay);
      
      if (window.location.hash == ("#" + commentName)) {
        // We're trying to navigate to this specific comment, so try it again:
        window.location = window.location
      }
    }
    
    insertComment(comment);
    
    if (isNewThread && canComment) {
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