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
          alert("Posted: " + result);
        },
        error: function(err) {
  	      alert("Error: " + err.responseText);
        }
      });    
    });
  }
  
  newConversationInput();

  convJsRoutes.controllers.ConversationController.getConversations(ownerId, spaceId, thingId).ajax({
    success: function (result) {
      alert("Existing Conversations: " + result);
	},
	error: function (err) {
	  alert("Error: " + err.responseText);
	}
  });
}