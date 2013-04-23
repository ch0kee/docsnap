
function  createSyncHelper(context, diffEngine) {
  return {
    createUpdatePackage: function() {
      var sentRevision = context.currentRevision.toString() + "[]"; //nothing to send
      console.log('sending ' + sentRevision);
      return sentRevision;
      //return { cmd : "update", args: sentRevision }      
    },    
    handleResponse: function(revision) {  
      var srvVersion = parseInt(revision.substr(1), 10);
      return {
        'o': function() { //checkout only
          var srvChangesIndex = revision.indexOf('[');
          var srvChanges = revision.substr(srvChangesIndex);
          
          context.syncContent = diffEngine.executeES1(context.syncContent, srvChanges);
          context.currentRevision = srvVersion;
          return { newContent: context.syncContent };
        }
      }
    }
  }
}

$(document).ready(function() {
  $("#editor").attr('contenteditable',false);
});

modified=false;
function setModified(modified) {
  modified = modified;
}






