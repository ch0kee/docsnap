
function  synchronizeContent() {
  var sentContent = actualContent();
  var sentChanges = diffEngine.getShortestEditScript(tokenize(syncContent), tokenize(sentContent) );
  //üres listát küldjünk, ha nem változott semmi
  var n=sentChanges.match(/^\[=[0-9]+\]$/);
  if (n == sentChanges) {
    sentChanges = "[]";
  }

  var sentRevision = currentRevision.toString() + sentChanges;
  console.log('sending ' + sentRevision);

  $.ajax({
    url     : "cupdate",
    type    : "POST",
    dataType: "html",
    cache   : false,
    data    : {
      d: sentRevision
    },
    success : function(revision) {
      console.log('received ' + revision);
      var respondType = revision[0];
      if (respondType == 'n') {
        console.log('no changes at all');
        if (revision.length != 1) {
          //error
        }
        setTimeout(synchronizeContent, syncInterval);
        return;
      }

      saveSelection();

      var srvVersion = parseInt(revision.substr(1), 10);
      var actContent = actualContent();
      var cliChanges = diffEngine.getShortestEditScript(tokenize(syncContent), tokenize(actualContent()));
      var oldSyncContent = syncContent;


      currentRevision = srvVersion;
      switch(respondType) {
        case 'd': //committed, dont have to do anything
          console.log('commit start');
//          syncContent = sentContent;
          syncContent = diffEngine.executeES1(syncContent, sentChanges);
          currentRevision = srvVersion;

          //so we can display the latest local changes instead of that
          console.log('b) executeES1 start');
          console.log('syncContent:' + syncContent);
          console.log('cliChanges:' + cliChanges);
          var actViewContent = diffEngine.executeES1(oldSyncContent, cliChanges);
          actualContent(actViewContent);
          console.log('commit done');
          break;
        case 'o': //checkout only
          console.log('checkout only');

          var srvChangesIndex = revision.indexOf('[');
          var srvChanges = revision.substr(srvChangesIndex);
          syncContent = diffEngine.executeES1(syncContent, srvChanges);

          //merge local changes with repository revision
          var actViewContent = diffEngine.executeES2(oldSyncContent, cliChanges, srvChanges);
          actualContent(actViewContent);
          break;
        default:
          //error
          break;
      }
      restoreSelection();
      setTimeout(synchronizeContent, syncInterval);
//      jumpToCaretSpan();
//      removeCaretSpan();
    },
    error : function( xhr, status ) {
      console.log("** error during synchronization");
    },
    complete : function( xhr, status ) {
      //alert("The request is complete!");
    }
  });
}

$(document).ready(function() {
  $("#editor").attr('contenteditable',false);
    //start loading progress bar
  $.ajax({
    url     : "/chello",
    type    : "POST",
    dataType: "html",
    cache   : false,
    data    : {
      d: "hello"
    },
    success : function(revision) {
      console.log("Kewl, we said hello!");
      console.log(revision);
      var srvVersion = parseInt(revision);
      var srvChangesIndex = revision.indexOf('[');
      var srvChanges = revision.substr(srvChangesIndex);
      syncContent = diffEngine.executeES1(syncContent, srvChanges);
      currentRevision = srvVersion;
      actualContent(syncContent);

      setTimeout(synchronizeContent, syncInterval);
    },
    error : function( xhr, status ) {
      console.log("Sorry, there was a problem!");
    },
    complete : function( xhr, status ) {
      //alert("The request is complete!");
    }
  });
});







