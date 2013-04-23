$(document).ready(function() {

  function actualContent(newContent) {
    if (actualContent.arguments.length == 0)
      return $('#editor').html();
    else
      $('#editor').html(newContent);
  }

  //var diffEngine = new DiffEngine();
  var diffEngine = new DiffEngine();
  var context = {
    syncContent:  ""
  , syncVersion: 0
  , getActualContent: function () { return $('#editor').html(); }
  };
  //var syncContent = "";
  //var currentRevision = 0; //0. revision is the empty content
  var applyCommittedChanges = true; //alkalmazzuk-e a módosításokat külön,
  var savedCaretPos = null;
  var syncInterval = 500;
  //vagy egyszerűen használjuk az elküldéskor érvényes tartalmat
  //(false is the way to go)
  
  //local content
  /*function  actualContent(newContent) {
    if (actualContent.arguments.length == 0)
      return $('#editor').html();
    else
      $('#editor').html(newContent);
  }  */
  
  var syncHelper = createSyncHelper(context, diffEngine);
  
  //synchronize
  function  synchronizeContent() {
    var updatePackage = syncHelper.createUpdatePackage();
    $.ajax({
      type    : "POST",
      dataType: "json",
      cache   : false,
      data    : {
        cmd:  "update"
      , args: JSON.stringify(updatePackage)
      },
      success : function(revision) {
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
          actualContent(result);
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
      success : function(revision) {
        console.log("Kewl, we said hello!");
        console.log(revision);
        var srvVersion = revision.version;
        context.syncContent = diffEngine.executeES1(context.syncContent, revision.edits);
        context.syncVersion = srvVersion;
        actualContent(context.syncContent);

        setTimeout(synchronizeContent, syncInterval);
      },
      error : function( xhr, status ) {
        console.log("Sorry, there was a problem!");
      },
      complete : function( xhr, status ) {
        //alert("The request is complete!");
      }
    });    
  } 
  
  initialCheckout();
});






