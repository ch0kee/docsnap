
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

function  synchronizeContent() {
  var sentContent = actualContent();
  var sentChanges = diffEngine.getShortestEditScript(syncContent, sentContent);
  //ne küldjük el, ha nem változott semmi
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
    success : function(revision) { // { 'data' : [ { 'value' : string, 'type' : char } ]
      console.log('received ' + revision);

      wrapCaretWithSpan();

      var actContent = actualContent();
      var cliChanges = diffEngine.getShortestEditScript(syncContent, actContent);

      var srvVersion = parseInt(revision);
      var srvChangesIndex = revision.indexOf('[');
      var checkoutOnly = srvChangesIndex != -1;
/*
      var authorRanges = json.authorRanges;
      authorRanges = { 'authorid' : [ (from, length) ] }
*/
      if (checkoutOnly) {
        //other authors made commits since last checkout
        //checkout only :(
        var srvChanges = revision.substr(srvChangesIndex);
        syncContent = diffEngine.executeES1(syncContent, srvChanges);
        currentRevision = srvVersion;

        //merge local changes with repository revision
        var actViewContent = diffEngine.executeES2(syncContent, cliChanges, srvChanges);
        actualContent(actViewContent);
      } else {
        //client was up-to-date and sent changes are the current server revision
        syncContent = diffEngine.executeES1(syncContent, sentChanges);
        currentRevision = srvVersion;

        //so we can display the latest local changes instead of that
        var actViewContent = diffEngine.executeES1(syncContent, cliChanges);
        actualContent(actViewContent);
      }
      jumpToCaretSpan();
      removeCaretSpan();
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

  $('#hello').click( function() {
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

     //   setInterval(synchronizeContent, 1000);
      },
      error : function( xhr, status ) {
        console.log("Sorry, there was a problem!");
      },
      complete : function( xhr, status ) {
        //alert("The request is complete!");
      }
    });
  });
});

function showPreview() {
  $('#preview').text( actualContent() );
}




