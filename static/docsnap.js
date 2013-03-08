
function wrapCaretWithSpan() {
    var sel = window.getSelection();
    if (sel.rangeCount <= 0)
        return;
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

var syncText = "";

//actText stores the filtered content
var actText = "";

function    synchronizeContent() {
    var sentText = actText();
    var sentSes = DiffEngine.getSES(syncText, sentText);
    $.ajax({
        url     : "/cupdate",
        type    : "POST",
        dataType: "json",
        cache   : false,
        data    : {
            d: JSON.stringify(sentSes)
        },
        success : function(json) { // { 'data' : [ { 'value' : string, 'type' : char } ]
            wrapCaretWithSpan();

            var localSes = DiffEngine.getSES(syncText, actText());
            var globalSes = DiffEngine.convertJsonToSes(json);
//            var newSyncText = DiffEngine.execute([localSes, globalSes], syncText);
//            syncText = newSyncText;
            syncText = DiffEngine.executeSES([localSes, globalSes], syncText);
//            syncText = newSyncText;
/*
            var applied1 = JsDiff.applyCharChanges1(syncText, serverDiff);
            var applied2 = JsDiff.applyCharChanges2(syncText, serverDiff, myDiffSinceSync);
  */

        },
        error : function( xhr, status ) {
            alert("Sorry, there was a problem!");
        },
        // code to run regardless of success or failure
        complete : function( xhr, status ) {
            //alert("The request is complete!");
        }
    });
}

function    actText() {
    return $('#editor').html();
}
