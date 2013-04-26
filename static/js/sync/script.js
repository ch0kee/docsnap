$(document).ready(function() {

  var diffEngine = new DiffEngine();
  var context = {
    syncContent:  ""
  , syncVersion: 0
  , syncSessionId: 0
  , getActualContent: function () { return $('#editor').html(); }
  };

  var syncInterval = 1000;
  var syncHelper = createSyncHelper(context, diffEngine);
  
  //synchronize
  function  synchronizeContent() {
    var updatePackage = syncHelper.createUpdatePackage();
    console.log('session id = ' + context.syncSessionId);
    var request = {sessionId: context.syncSessionId, revision: updatePackage};
    console.log('sending: ' + JSON.stringify(request));

    $.ajax({
      type    : "POST",
      dataType: "json",
      cache   : false,
      data    : {
        cmd:  "update"
      , args: JSON.stringify(request)
      },
      success : function(response) {
        console.log('received ' + JSON.stringify(response));
        var revision = response.revision;
        context.syncSessionId = response.sessionId;
        var nochanges = revision.version == context.syncVersion;
        if (nochanges) {
          //assert: revision.edits.length == 0
          setTimeout(synchronizeContent, syncInterval);
          return;
        }

        //assert srvVersion > syncVersion
        saveSelection();      
        
        var result = syncHelper.handleResponse(revision);
        if (result) {
          $('#editor').html(result);
        }
        
        restoreSelection();
        
        console.log('session id = ' + context.syncSessionId);
        setTimeout(synchronizeContent, syncInterval);
      },
      error : function( xhr, status ) {
        console.log("** error during synchronization");
      },
      complete : function( xhr, status ) {
        //alert("The request is complete!");
      }
    });
  }
  
  function  initialCheckout() {
      //start loading progress bar
    $.ajax({
      type    : "POST",
      dataType: "json",
      cache   : false,
      data    : {
        cmd: "init"
      , args: null
      },
      success : function(response) {
        context.syncContent = "";
        console.log(response);
        
        var revision = response.revision;
        var srvVersion = revision.version;
        context.syncContent = diffEngine.executeES1(context.syncContent, revision.edits);
        context.syncVersion = srvVersion;
        $('#editor').html(context.syncContent);
        context.syncSessionId = response.sessionId;
        setTimeout(synchronizeContent, syncInterval);
      },
      error : function( xhr, status ) {
        console.log("** error during synchronization");
      },
      complete : function( xhr, status ) {
        //alert("The request is complete!");
      }
    });    
  } 
  
  initialCheckout();
});






