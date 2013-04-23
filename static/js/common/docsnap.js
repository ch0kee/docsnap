layout = null;

$(document).ready(function() {

  function actualContent(newContent) {
    if (actualContent.arguments.length == 0)
      return $('#editor').html();
    else
      $('#editor').html(newContent);
  }
  $("#new").button();
  $("#share").button();
  $("#bold").button();
  $("#italic").button();
  
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
});






