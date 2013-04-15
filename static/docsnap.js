
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
savedCaretPos = null;
//vagy egyszerűen használjuk az elküldéskor érvényes tartalmat
//(false is the way to go)

// "<b>H</b>e&nbsp;<br>l" -->
// ["<b>", "H", "</b>", "e", "&nbsp;", "<br>", "l"]
function tokenize(data) {
  var tokens = new Array();
  var len = data.length;
  var idx = 0;
  var tokenizerRegex = new RegExp("<[^>]+>|&[a-z]+;|<\/[^>]+>", "g");
  var res = null;
  while ( (res = tokenizerRegex.exec(data)) ) {
    //megelozo karakterek 
    for(var i = idx; i < res.index; ++i) {
      tokens.push(data[i]);
    }

    tokens.push(res[0]);
    
    idx = res.index + res[0].length;
  }
  for(var i = idx; i < len; ++i) {
    tokens.push(data[i]);
  }
  return tokens;
}

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
        setTimeout(synchronizeContent, 1000);
        return;
      }
      
      saveSelection();
      
      var srvVersion = parseInt(revision.substr(1), 10);
      var actContent = actualContent();
      var cliChanges = diffEngine.getShortestEditScript(tokenize(syncContent), tokenize(actContent));
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
      setTimeout(synchronizeContent, 1000);
      restoreSelection();
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


var italicApplier;
var boldApplier;

$(document).ready(function() {
  rangy.init(); //css alkalmazásokhoz
  boldApplier = rangy.createCssClassApplier("ds_bold");
  italicApplier = rangy.createCssClassApplier("ds_italic");

  $('#sync').click( synchronizeContent );
  setInterval(showPreview, 200);

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

        setTimeout(synchronizeContent, 1000);
      },
      error : function( xhr, status ) {
        console.log("Sorry, there was a problem!");
      },
      complete : function( xhr, status ) {
        //alert("The request is complete!");
      }
    });
//  });

    $("#editor").on({
      //TAB must be handled here because keypress in
      //Chrome is already too late
      keydown: function(ev) {
        var code = ev.keyCode || ev.which;
        console.log(code);
        if (code == 9) {
        //TAB
          pasteHtmlAtSelection('&nbsp;&nbsp;&nbsp;&nbsp;');
          ev.preventDefault();
        }
      },
      
      keypress: function(ev){
        var code = ev.keyCode || ev.which;
        console.log(code);
        //ENTER
        if (code == 13) {
          pasteHtmlAtSelection('<br>');
          ev.preventDefault();
        }
      }
    });
    
    $("#bold").mousedown(function(e) {
      boldApplier.toggleSelection();
      e.preventDefault();
    });
    
    $("#italic").mousedown(function(e) {
      italicApplier.toggleSelection();
      e.preventDefault();
    });
});



function showPreview() {
  $('#preview').text( actualContent() );
}




