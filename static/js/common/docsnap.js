
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

layout = null;

$(document).ready(function() {
  $("#new").button();
  $("#share").button();
  $("#bold").button();
  $("#italic").button();
/*
  $( "#newdialog" ).dialog({
    autoOpen: true
  , modal: true
  , buttons: [ {text: "Create New Document", click: function() {
      window.location.href = 'cnew';
    }}]
  , draggable: false
  , height: 100
  , width: 300
  , resizable: false
  , dialogClass: 'no-close'
  });
*/
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






