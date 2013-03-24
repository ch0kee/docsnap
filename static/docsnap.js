

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



convertJsonToS
diffEngine = new DiffEngine();

synchronizedContent = "";
currentRevision = "";

function    synchronizeContent() {
  var sentContent = actualContent();
  var sentEditScript = diffEngine.getShortestEditScript(synchronizedContent, sentContent);
  //diffEngine.executeSES1(sentSes);
  var sentJSONData = { editscript: sentEditScript, currentrevision: currentRevision };
  console.log('sending' + JSON.stringify(sentJSONData));
  $.ajax({
    url     : "/cupdate",
    type    : "POST",
    dataType: "json",
    cache   : false,
    data    : {
      d: JSON.stringify(sentJSONData)
    },
    success : function(json) { // { 'data' : [ { 'value' : string, 'type' : char } ]
    /////
    success : function(revisions) { // { 'data' : [ { 'value' : string, 'type' : char } ]
      for(var i = 0; i < revisions.length; ++i) {
        synchronizedContent = diffEngine.executeSES1(synchronizedContent, revisions[i].editscript);
        currentRevision = revisions[i].id;
      }


    /////
      var globalSes = convertJsonToSes(json);
      var oldSyncContent = synchronizedContent;
      synchronizedContent = diffEngine.executeES2(synchronizedContent, sentEditScript, globalSes);

      wrapCaretWithSpan();
      var currentContent = actualContent();
      var currentSes = diffEngine.getShortestEditScript(oldSyncContent, currentContent);
      var mergedContent = diffEngine.executeES2(oldSyncContent, globalSes, currentSes);
      actualContent(mergedContent);
      jumpToCaretSpan();
      removeCaretSpan();
    },
    error : function( xhr, status ) {
      console.log("Sorry, there was a problem!");
    },
    complete : function( xhr, status ) {
      //alert("The request is complete!");
    }
  });
}

$(document).ready(function() {
  $.ajax({
      url     : "chello",
      type    : "POST",
      dataType: "json",
      cache   : false,
      data    : {
          d: "hello"
      },
      success : function(revisions) { // { 'revisions': [ 'id' : string, 'editscript':[ { 'value' : string, 'type' : char } ] ]}
        console.log("Kewl, we said hello!");
        synchronizedContent = "";
        for(var i = 0; i < revisions.length; ++i) {
          synchronizedContent = diffEngine.executeSES1(synchronizedContent, revisions[i].editscript);
          currentRevision = revisions[i].id;
        }
        setInterval(synchronizeContent, 1000);
      },
      error : function( xhr, status ) {
        console.log("Sorry, there was a problem!");
      },
      complete : function( xhr, status ) {
          //alert("The request is complete!");
      }
  });
});

function showPreview() {
 $('#preview').text( actualContent() );
}




