// Querki Conversations code
//
// This is intended for use with convPane.html, and not much anywhere else. Note that this
// depends on other script files loaded in convPane!

function loadConversations(ownerId, spaceId, thingId, convContainer, canComment) {
  convJsRoutes.controllers.ConversationController.getConversations(ownerId, spaceId, thingId).ajax({
    success: function (result) {
      alert("Existing Conversations: " + result);
	},
	error: function (err) {
	  alert("Error: " + err.responseText);
	}
  });
}