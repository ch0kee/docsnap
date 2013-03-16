

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


function    getActText() {
    return $('#editor').html();
}

function    setActText(t) {
    return $('#editor').html(t);
}

function    prepareJsonData(ses) {
    return { diff: ses };
}

diffEngine = new DiffEngine("");

setInterval(synchronizeContent, 5000);
function    synchronizeContent() {
    var sentText = getActText();
    var sentSes = diffEngine.getSES(sentText);
    //diffEngine.executeSES1(sentSes);
    var sentData = prepareJsonData( sentSes );
    $.ajax({
        url     : "/cupdate",
        type    : "POST",
        dataType: "json",
        cache   : false,
        data    : {
            d: JSON.stringify(sentData)
        },
        success : function(json) { // { 'data' : [ { 'value' : string, 'type' : char } ]
            wrapCaretWithSpan();
            var actText = getActText();
            var localSes = diffEngine.getSES(actText);
            var globalSes = diffEngine.convertJsonToSes(json);
            diffEngine.executeES2(localSes, globalSes);
            setActText(diffEngine._syncText); //ezt oninputban kellene csinalni
            jumpToCaretSpan();
        },
        error : function( xhr, status ) {
            //console.log("Sorry, there was a problem!");
        },
        // code to run regardless of success or failure
        complete : function( xhr, status ) {
            //alert("The request is complete!");
        }
    });
}


