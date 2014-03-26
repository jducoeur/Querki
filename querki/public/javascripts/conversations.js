// Querki Conversations code
//
// This is intended for use with convPane.html, and not much anywhere else. Note that this
// depends on other script files loaded in convPane!

function loadConversations(ownerId, spaceId, thingId, convContainer, canComment) {

  function newConversationInput() {
    var inputArea = $("#_addCommentTemplate").clone(true).attr('id', '_addComment');
    inputArea.show();
    convContainer.append(inputArea);
    
    var postButton = inputArea.find("._postCommentButton");
    var textArea = inputArea.find("._commentInput");
    
    postButton.click(function (evt) {
      var text = textArea.val();
      convJsRoutes.controllers.ConversationController.addComment(ownerId, spaceId, thingId, text, "").ajax({
        success: function(result) {
          alert("Posted comment: " + result);
        },
        error: function(err) {
  	      alert("Error: " + err.responseText);
        }
      });    
    });
  }
  
  newConversationInput();

  convJsRoutes.controllers.ConversationController.getConversations(ownerId, spaceId, thingId).ajax({
    success: function (convs) {
      _.each(convs, function(node) {
        var comment = node.comment;
        var id = comment.id;
        
        var threadDisplay = $("#_convThreadTemplate").clone(true).attr('id', '_convThread' + id);
        threadDisplay.show();
        convContainer.append(threadDisplay);
        
        var author = comment.authorId;
        // Note that, for the date, we use the moment library, which is loaded by convPane:
        var created = moment(comment.createTime);
        var createdDisplay = created.calendar();
        var text = comment.text;
        var commentDisplay = $("#_commentTemplate").clone(true).attr('id', '_comment' + id)
        commentDisplay.find("._commentLink").prop('name', 'comment' + id);
        commentDisplay.find("._commentAuthor").html(author);
        commentDisplay.find("._commentTime").html(createdDisplay);
        commentDisplay.find("._commentText").html(text);
        commentDisplay.show();
        threadDisplay.append(commentDisplay);
        
        // TODO: deal recursively with node.responses, inside the threadDisplay!
      });
	},
	error: function (err) {
	  alert("Error: " + err.responseText);
	}
  });
}