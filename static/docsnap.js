goog.require('goog.asserts');
goog.require('goog.debug.Logger');


function wrapCaretWithSpan() {
  var sel = window.getSelection();
  if (!sel || sel.rangeCount <= 0) {
    console.log('no selection');
    return;
  }
  removeCaretSpan();
  var range = sel.getRangeAt(0);
  var span = document.createElement("span");
  span.setAttribute("id", "caretspan");
  range.surroundContents(span);
  sel.selectAllChildren(span);
  //sel.removeAllRanges();
  //sel.addRange(range);
}

function jumpToCaretSpan() {
  $('#editor').focus();
  var sel = window.getSelection();
  if (!sel) {
    console.log('no selection');
    return;
  }
  var span = document.getElementById("caretspan");
  if (span) {
    sel.selectAllChildren(span);
  }
}

function    removeCaretSpan() {
  if ($('#caretspan').length > 0) {
    if ($('#caretspan').contents().length > 0) {
      $('#caretspan').contents().unwrap();
    } else {
      $('#caretspan').remove();
    }
  }
}

function  actualContent(newContent) {
  if (actualContent.arguments.length == 0)
    return $('#editor').html();
  else
    $('#editor').html(newContent);
}

diffEngine = new DiffEngine();

syncContent = "";
currentRevision = 0; //0. revision is the empty content
applyCommittedChanges = true; //alkalmazzuk-e a módosításokat külön,
//vagy egyszerűen használjuk az elküldéskor érvényes tartalmat
//(false is the way to go)

function  synchronizeContent() {
  var sentContent = actualContent();
  var sentChanges = diffEngine.getShortestEditScript(syncContent, sentContent);
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
        return;
      }

//      wrapCaretWithSpan();
      var srvVersion = parseInt(revision.substr(1), 10);
      var actContent = actualContent();
      var cliChanges = diffEngine.getShortestEditScript(syncContent, actContent);
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
  $('#sync').click( synchronizeContent );

//  $('#hello').click( function() {
    //start loading progress bar
    $.ajax({
      url     : "chello",
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

        setInterval(synchronizeContent, 1000);
      },
      error : function( xhr, status ) {
        console.log("Sorry, there was a problem!");
      },
      complete : function( xhr, status ) {
        //alert("The request is complete!");
      }
    });
//  });
});

function showPreview() {
  $('#preview').text( actualContent() );
}




