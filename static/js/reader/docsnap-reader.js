
//@ reader createSyncHelper
function  createSyncHelper(context, diffEngine) {
  return {
    //@ nem küldünk be semmit
    createUpdatePackage: function() {
      console.log('sending ' + sentRevision);
      return { version: context.syncVersion, edits [] };    
    },
    //@ csak checkout-ot kezelünk
    handleResponse: function(revision) { 
      context.syncContent = diffEngine.executeES1(context.syncContent, revision.edits);
      context.syncVersion = revision.version;
      return context.syncContent;
    }
  }
}

$(document).ready(function() {
  $("#editor").attr('contenteditable',false);
});







