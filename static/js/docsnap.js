
//@ dokumentum előkészítése
$(document).ready(function() {

  rangy.init(); 

  //@ New
  $("#newbtn").button()
    .click(function() {
      DocSnap.showQuestionDialog("Are you sure you want to create a new document?",
        function() {
          window.location.href = '/new';
        });
    });
    
  //@ Share
  $("#sharebtn").button()
    .click(function() {
      $("#exportmenu").hide();
      $("#sharemenu").show('fast').position({
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
      DocSnap.sendAjaxCommand("share", { type: shareType }, function(rsp) {
        console.log(rsp);
        DocSnap.showInformationDialog("Send this link to your friend:\n"+rsp.link);
      });
    });
  
  //@ Export
  $("#exportbtn").button()
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
      DocSnap.sendAjaxCommand("export", {index: parseInt(index) }, function(rsp) {
        DocSnap.downloadFile(rsp.url);
      });
    });  
  
  $('#separator').button({disabled:true});
  
  //@ Formatting
  var formattingAppliers = {};
  $('.formatting').each(function() {
    var styleToApply = $(this).attr('data-class');
    formattingAppliers[ styleToApply ] = rangy.createCssClassApplier(styleToApply, { applyToEditableOnly: true, normalize: true });
    
    $(this).button()
      .mousedown(function(e) {
        formattingAppliers[ styleToApply ].toggleSelection();
        e.preventDefault();
      });
    });

  var layout = $('body').layout({
		north__resizable: false
		,	east__size:	200
    , south__minSize: 30
    , south__maxSize: 30
    , north__minSize: 40
    , north__maxSize: 40
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
		,	showDebugMessages: false
		, center__contentSelector:	'div.editor'
		, east__contentSelector: 'div.chatlog'

  });
  
});






