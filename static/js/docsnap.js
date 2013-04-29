
var layout = null;




//@ dokumentum előkészítése
$(document).ready(function() {
  $("#new").button();
  $("#share").button()
    .click(function() {
      $("#exportmenu").hide();
      $("#sharemenu").show('slow').position({
            my: "left top",
            at: "left bottom",
            of: this
          });
      //tüntessük el a menüt akárhova kattintunk
      $( document ).one( "click", function() {
        $("#sharemenu").hide();
      });
      return false;
    });
  $("#sharemenu").menu().hide();
  $('#sharemenu').children()
    .css('cursor', 'pointer')
    .click(function() {
      var shareType = $(this).attr('data-type');
      console.log('share as '+shareType);
      DocSnap.sendAjaxCommand("share", shareType, function(revision) {
          alert(revision);
      });
    });

  $("#export").button()
    .click(function() {
      $("#sharemenu").hide();
      $("#exportmenu").show('slow').position({
            my: "left top",
            at: "left bottom",
            of: this
          });
      //tüntessük el a menüt akárhova kattintunk
      $( document ).one( "click", function() {
        $("#exportmenu").hide();
      });
      return false;
    });
  $("#exportmenu").menu().hide();
  $("#exportmenu").children()
    .css('cursor', 'pointer')
    .click(function() {
      var index = $(this).attr("data-index");
//      window.location.href = '/export';
      DocSnap.sendAjaxCommand("export", index, function(data) {
        //e.preventDefault();
       // alert(data);
        downloadFile(data);
      });
    });
       
  $("#bold").button();
  $("#italic").button();
  
  //elrendezés beállítása
  layout = $('body').layout({
		north__resizable: false
		,	east__size:	200
    , south__minSize: 30
    , south__maxSize: 30
    , north__minSize: 70
    , north__maxSize: 70
    , north__spacing_open: 0
		, south__resizable: false
    , south__spacing_open: 0
		,	east__spacing_open: 10
		,	east__spacing_closed: 20
		,	east__resizable: false
    , east__closable: true
    , south__closable: false
		,	center__minWidth:	100
		,	east__fxSpeed_open: 300
		,	east__fxSpeed_close: 300
		,	stateManagement__enabled: true
		, enableCursorHotkey: false
		,	showDebugMessages: false
  });

  //szövegrészletek manupilálásához
  rangy.init(); 
});






