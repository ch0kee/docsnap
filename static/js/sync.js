$(document).ready(function() {

  var diffEngine = new DiffEngine();
  var context =
    { syncContent:  ""
    , syncVersion: 0
    , syncChatBuffer: []
    , syncChatVersion: -1
    , getActualContent: function () { return $('#editor').html(); } };

  var syncInterval = 1000;
  var syncHelper = createSyncHelper(context, diffEngine);
  
  //synchronize
  function  synchronizeContent() {
    var updatePackage = syncHelper.createUpdatePackage();
    var request =
      { reqRevision: updatePackage
      , reqChatBuffer: context.syncChatBuffer
      , reqChatVersion: context.syncChatVersion };
    console.log('synchronizeContent()::sending: ' + JSON.stringify(request));
    context.syncChatBuffer = [];
    $.ajax({
      type    : "POST",
      dataType: "json",
      cache   : false,
      data    : {
        cmd:  "update"
      , args: JSON.stringify(request)
      },
      success : function(response) {
        console.log('synchronizeContent()::received ' + JSON.stringify(response));
        var revision = response.rspRevision;
        context.syncChatVersion = response.rspChatVersion;
        console.log('synchronizeContent()::context ' + JSON.stringify(context));
        showChatMessages(response.rspChatMessages);
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
  
  
  $("#chatfield").keypress(function(e) {
    if (e.which == 13) {
      console.log('chat: '+$("#chatfield").val());
      context.syncChatBuffer.push($("#chatfield").val());
      $("#chatfield").val('');
      return false; 
    }
  });
  
  function  showChatMessages(msgs) {
    for(var i = msgs.length-1; i >= 0; --i) {
      $('#chatlog').append('<div>'+msgs[i].sender+': '+msgs[i].message+'</div>');
    }
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
        console.log('initialCheckout()::received ' + JSON.stringify(response));
        
        var revision = response.rspRevision;
        var srvVersion = revision.version;
        context.syncContent = diffEngine.executeES1(context.syncContent, revision.edits);
        context.syncVersion = srvVersion;
        context.syncChatVersion = response.rspChatVersion;
        showChatMessages(response.rspChatMessages);
        $('#editor').html(context.syncContent);
        console.log('initialCheckout()::context ' + JSON.stringify(context));
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






