
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
syncInterval = 5000;
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


var italicApplier;
var boldApplier;

var layout;

$(document).ready(function() {
  $("#new").button();
  $("#share").button();
  $("#bold").button();
  $("#italic").button();

  $( "#newdialog" ).dialog({
    autoOpen: true
  , modal: true
  , buttons: [ {text: "Create New Document", click: function() {
      window.location.href = 'cnew';
/*
  //todo: disable interaction until response
      $.ajax({
        url     : "/cnew",
        type    : "POST",
        dataType: "html",
        cache   : false,
        data    : {
          d: "hello"
        },
        success : function(ret) {
          console.log("???");
        },
        error : function( xhr, status ) {
          console.log("???");
        },
        complete : function( xhr, status ) {
          console.log("???");
        }
      });*/      
    }}]
  , draggable: false
  , height: 100
  , width: 300
  , resizable: false
  , dialogClass: 'no-close'
  });
  //$( "#opener" ).click(function() {
  //  $( "#dialog" ).dialog( "open" );
  //});


  layout = $('body').layout({
		north__resizable:			false
    , north__minSize: 70
    , north__maxSize: 70
    , north__spacing_open:		0
		, south__resizable:			false
    , south__spacing_open:		0
		,	east__spacing_open:		10
		,	east__spacing_closed:		20
		,	east__size:					200
		,	east__resizable:			false
    , east__closable: true
    , south__closable: false
    , south__minSize:30
    , south__maxSize:30
//		,	east__minSize:				200
//		,	east__maxSize:				200 // 50% of layout width
		,	center__minWidth:			100
		//	some pane animation settings
		,	east__animatePaneSizing:	false
		,	east__fxSpeed_size:			"fast"	// 'fast' animation when resizing west-pane
		,	east__fxSpeed_open:			1000	// 1-second animation when opening west-pane
		,	east__fxSettings_open:		{ easing: "easeOutBounce" } // 'bounce' effect when opening
		,	east__fxSpeed_close:			1000	// 1-second animation when opening west-pane
//		,	east__fxName_close:			"none"	// NO animation when closing west-pane

		//	enable state management
		,	stateManagement__enabled:	true // automatic cookie load & save enabled by default

		,	showDebugMessages:			false // log and/or display messages from debugging & testing code
  });

  rangy.init(); //css alkalmazásokhoz
  boldApplier = rangy.createCssClassApplier("ds_bold");
  italicApplier = rangy.createCssClassApplier("ds_italic");


//  $('#hello').click( function() {
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
  
  /*
  var jqxhr = $.post("example.php", function() {
alert("success");
})
.done(function() { alert("second success"); })
.fail(function() { alert("error"); })
.always(function() { alert("finished"); });
  */
  
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
    },

    input: function() {
      setModified(true);
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
  
  //$("#editor").focus();
  $("#newdialog").focus();
  $("#newdialog").dialog("option", "position", "center");
  $(window).resize(function() {
    $("#newdialog").dialog("option", "position", "center");
  });
});

modified=false;
function setModified(modified) {
  modified = modified;
}






