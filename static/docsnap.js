
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
syncInterval = 400;
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
  layout = $('body').layout({

		//	reference only - these options are NOT required because 'true' is the default
			closable:					true	// pane can open & close
		,	resizable:					true	// when open, pane can be resized
		,	slidable:					true	// when closed, pane can 'slide' open over other panes - closes on mouse-out
		,	livePaneResizing:			true

		//	some resizing/toggling settings
		,	north__slidable:			false	// OVERRIDE the pane-default of 'slidable=true'
		,	north__togglerLength_closed: '100%'	// toggle-button is full-width of resizer-bar
		,	north__spacing_closed:		20		// big resizer-bar when open (zero height)
		,	south__resizable:			false	// OVERRIDE the pane-default of 'resizable=true'
		,	south__spacing_open:		0		// no resizer-bar when open (zero height)
		,	south__spacing_closed:		20		// big resizer-bar when open (zero height)

		//	some pane-size settings
		,	west__minSize:				100
		,	east__size:					300
		,	east__minSize:				200
		,	east__maxSize:				.5 // 50% of layout width
		,	center__minWidth:			100

		//	some pane animation settings
		,	west__animatePaneSizing:	false
		,	west__fxSpeed_size:			"fast"	// 'fast' animation when resizing west-pane
		,	west__fxSpeed_open:			1000	// 1-second animation when opening west-pane
		,	west__fxSettings_open:		{ easing: "easeOutBounce" } // 'bounce' effect when opening
		,	west__fxName_close:			"none"	// NO animation when closing west-pane

		//	enable showOverflow on west-pane so CSS popups will overlap north pane
		,	west__showOverflowOnHover:	true

		//	enable state management
		,	stateManagement__enabled:	true // automatic cookie load & save enabled by default

		,	showDebugMessages:			true // log and/or display messages from debugging & testing code
  });
  $( "#tabs" ).tabs();

  rangy.init(); //css alkalmazásokhoz
  boldApplier = rangy.createCssClassApplier("ds_bold");
  italicApplier = rangy.createCssClassApplier("ds_italic");

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

        setTimeout(synchronizeContent, syncInterval);
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
});

modified=false;
function setModified(modified) {
  modified = modified;
}


function showPreview() {
  $('#preview').text( actualContent() );
}




